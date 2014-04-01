# Fuse — A Circuit Breaker implementation for Erlang

This application implements a so-called circuit-breaker for Erlang.

[![Build Status](https://travis-ci.org/jlouis/fuse.png?branch=master)](https://travis-ci.org/jlouis/fuse)

# Introduction

When we build large systems, one of the problems we face is what happens when we have long dependency chains of applications. We might have a case where applications call like this:

	app_A → app_B → app_C
	
Now, if we begin having errors in application `B` down the road, the problem is that application `A` needs to handle this by waiting for a timeout of Application B all the time. This incurs latency in the code base. A Circuit Breaker detects the error in the underlying system and then avoids making further queries. This allows you to handle the breakage systematically in the system. For long cascades, layering of circuit breakers allow one to detect exactly which application is responsible for the breakage.

A broken circuit introduces some good characteristics in the system:

* There is no buffer/queue buildup since requests can get passed immediately. No waste of resources is had.
* returning from a broken circuit has favorable latency close to 0μs. This allows code to try a backup system quickly, or to give negative feedback.
* Clients can discriminate a system with slow response time from one that is genuinely broken. For the vast majority of clients, this is beneficial. A front-end can opt to skip displaying of certain elements, should the backend parts be down.

The broken circuit will be retried once in a while. The system will then auto-heal if connectivity comes back for the underlying systems. 

# Thanks

Several companies should be thanked:

* Issuu: for needing this tool in the first place, and lending some time to build it.
* Erlang Solutions: For lending some development time to the project as well.
* Quviq: for making Erlang QuickCheck and helping out with model construction.

## Contributors:

List of people who have made contributions to the project of substantial size:

* Jesper Louis Andersen
* Thomas Arts

# Tutorial

To use fuse, you must first start the fuse application:

	application:start(fuse).
	
but note that in real systems it is better to have other applications *depend* on fuse and then start it as part of a release boot script. Next, you must install a fuse into the system by *installing* a fuse description. This is usually done as part of the `application:start/1` callback:

	Name = database_fuse
	Strategy = {standard, MaxR, MaxT},
	Refresh = {reset, 60000},
	Opts = {Strategy, Refresh},
	fuse:install(Name, Opts).
	
This sets up a *fuse* with a given Name and a given set of options. Options are given as a tuple with two values. The *strategy* of the fuse and the *refresh* of the fuse.

* Strategy denotes what kind of fuse we have. The default is a `standard` fuse. Such a fuse will tolerate `MaxR` melting attempts in a time window of `MaxT`.
* Refresh tells what to do with the fuse once it melts. Here we say that the fuse will reset after 60000ms.

Fuses are name-created idempotently, so your application can recreate a fuse if it wants. Note however, that fuse recreation has two major rules:

* Reinstalling a fuse resets its internal state.
* Reinstalling a fuse can reset its options.

So re-creation of a fuse overwrites the existing fuse.

Once you have installed a fuse, you can use it in one of two ways:

	case fuse:ask(database_fuse, [sync]) of
		ok -> …;
		blown -> …
	end,
        
	case fuse:ask(database_fuse) of
		ok -> …;
		blown -> …
	end,

This queries the fuse for its state and lets you handle the case where it is currently blown. The second variant does the same call, but does so in a *dirty* way. This means while being faster, you may see an an `ok` answer from a fuse that is in the process of being blown. In most cases this race condition is benign to real software.

Now suppose you have a working fuse, but you suddenly realize you get errors of the type `{error, timeout}`. Since you think this is a problem, you can tell the system that the fuse is under strain. You do this by *melting* the fuse:

	case emysql:execute(Stmt) of
	    {error, connection_lock_timeout} ->
	    	ok = fuse:melt(database_fuse),
	    	…
	    …
	end,
	
The fuse has a policy, so once it has been melted too many times, it will blow for a while until it has heated down. Then it will heal back to the initial state. If the underlying system is still broken, the fuse will quickly break again. While this reset-methodology is not optimal, it is easy to create a Quickcheck model showing the behaviour is correct.Note `melt` is synchronous. It blocks until the fuse can handle the melt. There are two reasons for this:

* It is overload-safe against the fuse code. Even if processes can outrun the fuse, it cannot build up queue due to this (though this is only the case if there is a bounded number of accessors to the fuse).
* It is on the slow-path. When we melt, we are in a bad situation. So waiting a bit more before given an answer back is probably not going to be a problem. We picked this choice explicitly in order to make sure it works under load.

Another way to run the fuse is to use a wrapper function. Suppose you have a function with the following spec:

	-spec exec() -> {ok, Result} | {melt, Result}
	  when Result :: term().

	%% To use this function:
	case fuse:run(Name, fun exec/0, [sync]) of
		{ok, Result} -> …;
		blown -> …
	end,

	case fuse:run(Name, fun exec/0) of
		{ok, Result} -> …;
		blown -> …
	end,

this function will do the asking and melting itself based on the output of the underlying function. The `sync` variant does so synchronously, while the simpler variant is subject to (benign) races like in the above example. The `run/2,3` invocation is often easier to handle in programs.

## Monitoring fuse state

Fuses installed into the system are automatically instrumented in two ways, `folsom` and the `alarm_handler`.

A fuse named `foo` reports to `folsom` with the following stats:

* Two spirals: `foo.ok` and `foo.blown` which are increased whenever someone `ask/1`'s the fuse.
* A meter: `foo.melt` whenever the fuse is melted.

Furthermore, fuses raises alarms when they are blown. They raise an alarm under the same name as the fuse itself. To clear the alarm, the system uses hysteresis. It has to see 3 consecutive `ok` states on a fuse before clearing the alarm. This is to avoid alarm states from flapping excessively.

# Tests

Fuse is written with two kinds of tests. First of all, it uses a set of Common Test test cases which runs the basic functionality of the system. Furthermore, fuse is written with Erlang QuickCheck test cases. EQC tests are written before the corresponding code is written, and as such, this is "Property Driven Development".

To run the standard tests, execute:

	make tests
	
To run the EQC tests, you have to

	cd eqc_test
	make console
	
And then in the Erlang console, you can execute

	make:all([load]).
	error_logger:tty(false). % Shut up the error logger while running tests
	eqc:module(fuse_eqc).

I am deliberately keeping them out of the travis build due to the necessity of Erlang Quickcheck in order to be able to run tests. There are a set of models, each testing one aspect of the fuse system. Taken together, they provide excellent coverage of the fuse system as a whole.

Great care has been taken in order to make sure fuse can be part of the error kernel of a system. The main fuse server is not supposed to crash under any circumstance. The monitoring application may crash since it is only part of the reporting. While important, it is not essential to correct operation.

## EQC Test harness features:

* Tests the fuse API in all cases with positive testing
* Uses negative testing to make sure the `install/2` command rejects wrong options correctly.
* Uses negative testing to make sure return values are correct for every other command
* Uses sequential testing to make sure command invocation is sane.
* Uses parallel testing to make sure there are no race conditions, even when many clients call into the system at the same time. The assumption is all calls are made with the synchronous API. For most practical uses, one can skip synchronous `ask/1` calls and use the async variants.
* Uses EQC PULSE to randomize the schedule of the processes we run to make sure they are correct.
* Models time in EQC and controls time advancement to test for situations where timing is a problem in the system under test.
* Uses EQC Component to monitor correct handling of alarms triggering and clearing in the Erlang system with correct hysteresis.

Furthermore:

* The EQC Test harness tests its internal consistency of time handling by using properties to test its own model for correctness.
* Makes a case as to why timestamping should happen inside `fuse_srv` and not outside (clocks can skew and time can go backwards if clients draw from a time source. In a distributed setting, it is even worse).

# Subtle Errors found by EQC

Software construction is a subtle and elusive business. Most errors in software are weeded out early in the development cycle, the errors that remain are really rare and hard to find. Static type systems will only raise the bar and make the remaining errors even more slippery from your grasp. Thus, the only way to remove these errors is to use a tool which is good at construction elusive counter-examples. Erlang QuickCheck is such a tool.

Development guided by properties leads to a code base which is considerably smaller. In the course of building `fuse` we iteratively removed functionality from the code base which proved to be impossible to implement correctly. Rather than ending up with a lot of special cases, development by property simply suggested the removal of certain nasty configurations of the software, which does nothing but makes it more bloated. Worse—one could argue some of these configuration would never be used in practice, leading to written code which would never be traversed.

## General:

* Numerous small mistakes have been weeded out while developing the code.
* EQC has guided the design in a positive way. This has lead to smaller and simpler code with fewer errors.
* EQC has suggested improvements to the API. Specifically in the area of synchronous calls and race condition avoidance.

## Subtleties:

* If you `install/2` a fuse with an intensity of `0` it will start in the `blown` state and not in the `ok` state. The code did not account for this small detail.
* Parallel test case generation found a wrong reset invocation where the answer was `{error, no_such_fuse}` and not the specified `{error, not_found}`. Sequential tests did not find this particular interleaving problem. Subsequently, the discovery was an inadequacy in the sequential model with too weak pre-condition generation.
* More work on the model made it clear the fuse intensity of '0' requires much special handling in the code base and model to handle correctly. It was decided to reject an intensity of 0 altogether and shave off much complexity in the implementation and in the model.
* EQC and careful consideration came up with the idea to separate the alarm handling code from the fuse handling code in the system, to protect a faulty alarm handler from taking down the fuse system.
* EQC, using PULSE to test, figured out we need a way to synchronize `ask/1`. The problem is that this runs outside the `fuse_srv` which leads the parallel race conditions. This was mitigated by adding a variant, `ask/2` which is sync-safe and poses no race conditions.
* EQC, using parallel testing, uncovered a problem with the synchronicity of `run/2`.
* EQC, and helpful hints by Thomas Arts, made it evident that the method used to draw timestamps was incorrect. A new model, where timestamps are generated inside the fuse system was much easier to test and verify.
* EQC made it clear exactly how time is used in the system.

The monitor model found the following:

* Monitor handling was incorrect and the hysteresis was not implemented correctly, leading to numerous flaps on the alarms.

