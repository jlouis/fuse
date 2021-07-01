# Fuse — A Circuit Breaker implementation for Erlang

This application implements a so-called circuit-breaker for Erlang.

[![Build Status](https://github.com/jlouis/fuse/workflows/build/badge.svg)](https://github.com/jlouis/fuse)

*NOTE*: If you need to access FUSE (Filesystem in Userspace) then this is not the project you want. An Erlang implementation can be found in the *fuserl* project, [fuserl on Google Code](https://code.google.com/p/fuserl/) or [fuserl on Github](https://github.com/tonyrog/fuserl) gives the pointers.

Fuse has seen use in various production systems, and the code should be quite stable. In particular, the extensive QuickCheck testing should make the code trustable to a larger extent than its production use.

## Alternative implementations

I know of a couple of alternative circuit breaker implementations for Erlang:

* [breaky](https://github.com/mmzeeman/breaky) - Breaky is implemented as an FSM in front of the protected service. This is another implementation model in which the breaker also controls service restarts once they fail. As such, it implements a slightly different pattern than *fuse*. In breaky, you have a process which can fail and you have a restart policy for that process. *breaky* will then handle the automatic restart policy and do circuit breaking handling for you. It is used in systems like Zotonic to handle cascading errors there.

* [circuit breaker](https://github.com/klarna/circuit_breaker) - Klarna's battle-tested circuit breaker application. The pattern here is much closer to what fuse provides. However, the main difference is that in circuit_breaker, there can be different thresholds for different error types. This means you can be more specific as to what kind of error you have and how you plan on handling it. This tool also has the advantage of 8 years of battle-testing in production at Klarna. It may fit your use case if you can tolerate spawning a function per call to the circuit breaker application.

## Changelog

We use semantic versioning. In release `X.Y.Z` we bump

* `X` whenever we break backwards compatibility
* `Y` whenever we add additional—but backwards compatible—functionality
* `Z` whenever we do a point release fixing bugs

### 2.5.0

* Feature: Allow the `MaxR` value in standard fuses to be 0 (Danila Fediashchin)

### 2.4.5

* Fix: fuse_monitor crashes when using fault injection fuses.

### 2.4.4

Maintenance. No functional changes to the code, but tests were updated.

### 2.4.3

Another maintenance release, but with one feature

* Feature: a fuse is now identified by `term()` rather than `atom()`. This has also been tested to
  work as expected.
* Finally dropped Erlang/OTP release 16 and earlier support from time handling.
* EQC test cases have been moved to their own directory, which should clean up some handling.

### 2.4.2

Just a simply maintenance release for [Hex](https://hex.pm)

### 2.4.1

Maintenance release. Several grave errors were removed due to the extension of the QuickCheck model to also include timing:

* Timer handling could lead to a (benign) situation where a timer was added twice.
* Timers and `circuit_disable/1` / `circuit_enable/1` were not playing well together.
* Make the test support reset periods between 1ms and 60000ms.
* Make the test support windows between 1ms and 30000ms.

### 2.4.0

Add support for monitoring fuses through prometheus.io. Contribution by Ilya Khaprov.

### 2.3.0

Support the `fault_injection` style fuses. These are fuses that fails automatically at a certain rate, say 1/500 requests, to test systems for robustness against faulty data.

### 2.2.0

Add `fuse:circuit_disable/1` and `fuse:circuit_enable/1`.

### 2.1.0

Add the ability to remove a fuse. Work by Zeeshan Lakhani / Basho.

### 2.0.0

This major release breaks backwards compatibility in statistics. Cian Synott wrote code which generalizes stats collecting through plugins, with `exometer` and `folsom` being the major plugins. Read about how to configure the system for exometer or folsom use in this README file.

No other changes in this version.

### 1.1.0

Rename `fuse_evt` → `fuse_event`. While this is not strictly a valid thing, since we break backwards compatibility, I hope no-one have begun using Fuse yet. As such, I decided to make this a minor bump instead.

### 1.0.0

Initial Release.

## Background

When we build large systems, one of the problems we face is what happens when we have long dependency chains of applications. We might have a case where applications call like this:

  app_A → app_B → app_C

Now, if we begin having errors in application `B`, the problem is that application `A` needs to handle this by waiting for a timeout of Application `B` all the time. This incurs latency in the code base. A Circuit Breaker detects the error in the underlying system and then avoids making further queries. This allows you to handle the breakage systematically in the system. For long cascades, layering of circuit breakers allow one to detect exactly which application is responsible for the breakage.

A broken circuit introduces some good characteristics in the system:

* There is no buffer/queue buildup since requests can get passed immediately. No waste of resources is had.
* Returning from a broken circuit has favorable latency close to 0μs. This allows code to try a backup system quickly, or to give negative feedback.
* Clients can discriminate a system with slow response time from one that is genuinely broken. For the vast majority of clients, this is beneficial. A front-end can opt to skip displaying of certain elements, should the backend parts be down.
* Circuits introduce a point where your cascading dependencies can be easily monitored. By moving monitoring and reporting to another subsystem, one achieves decoupling between the system doing operation and the system overseeing operation. This is usually nice from an architectural perspective.

The broken circuit will be retried once in a while. The system will then auto-heal if connectivity comes back for the underlying systems.

## Thanks

Several companies should be thanked:

* Issuu: for needing this tool in the first place, and lending some time to build it.
* Erlang Solutions: For lending some development time to the project as well.
* Quviq: for making Erlang QuickCheck and helping with model construction.

## Contributors

List of people who have made contributions to the project of substantial size:

* Cian Synnott
* Ilya Khaprov
* Jesper Louis Andersen
* Thomas Arts
* Zeeshan Lakhani

## Documentation

Read the tutorial in the next section. For a command reference, there is full EDoc documentation via `make docs`. Note that great care has been taken to produce precise documentation of the stable API fragment of the tool. If you find anything to be undocumented, please open an Issue—or better: a pull request with a patch!

## Tutorial

To use fuse, you must first start the fuse application:

```erlang
application:start(fuse).
```

but note that in real systems it is better to have other applications *depend* on fuse and then start it as part of a release boot script. Next, you must add a fuse into the system by *installing* a fuse description. This is usually done as part of the `application:start/1` callback:

```erlang
Name = database_fuse
Strategy = {standard, MaxR, MaxT}, %% See below for types
Refresh = {reset, 60000},
Opts = {Strategy, Refresh},
fuse:install(Name, Opts).
```

This sets up a *fuse* with a given Name and a given set of options. Options are given as a tuple with two values. The *strategy* of the fuse and the *refresh* of the fuse.

* Strategy denotes what kind of fuse we have. The default is a `standard` fuse. Such a fuse will tolerate `MaxR` melting attempts in a time window of `MaxT`.
* Refresh tells what to do with the fuse once it melts. Here we say that the fuse will reset after 60000ms.

Fuses are name-created idempotently, so your application can recreate a fuse if it wants. Note however, that fuse recreation has two major rules:

* Reinstalling a fuse resets its internal state.
* Reinstalling a fuse overwrites the options.

Once you have installed a fuse, you can ask about its state:

```erlang
Context = sync,
case fuse:ask(database_fuse, Context) of
  ok -> …;
  blown -> …
end,
```

This queries the fuse for its state and lets you handle the case where it is currently blown. The `Context` specifies the context under which the fuses is running (like in mnesia). There are currently two available contexts:

* `sync` - call the fuse synchronously. This is the safe way where each call is factored through the fuse server. It has no known race conditions.
* `async_dirty` - A fast call path, which circumvents the single `fuse_server` process. It is much faster, but has been known to provide rare races in which parallel processes might observe the wrong values. In other words, with `Context = async_dirty` the calls are not linearizible.

Now suppose you have a working fuse, but you suddenly realize you get errors of the type `{error, timeout}`. Since you think this is a problem, you can tell the system that the fuse is under strain. You do this by *melting* the fuse:

```erlang
case emysql:execute(Stmt) of
    {error, connection_lock_timeout} ->
      ok = fuse:melt(database_fuse),
      …
    …
end,
```

The fuse has a policy, so once it has been melted too many times, it will blow for a while until it has cooled down. Then it will heal back to the initial state. If the underlying system is still broken, the fuse will quickly break again. While this reset-methodology is not optimal, it is easy to create a Quickcheck model showing the behaviour is correct. Note `melt` is synchronous. It blocks until the fuse can handle the melt. There are two reasons for this:

* It is overload-safe against the fuse code. Even if processes can outrun the fuse, it cannot build up queue due to this (though this is only the case if there is a bounded number of accessors to the fuse).
* It is on the slow-path. When we melt, we are in a bad situation. So waiting a bit more before given an answer back is probably not going to be a problem. We picked this choice explicitly in order to make sure it works under load.

Another way to run the fuse is to use a wrapper function. Suppose you have a function with the following spec:

```erlang
-spec exec() -> {ok, Result} | {melt, Result}
  when Result :: term().

%% To use this function:
Context = sync,
case fuse:run(Name, fun exec/0, Context) of
  {ok, Result} -> …;
  blown -> …
end,
```

this function will do the asking and melting itself based on the output of the underlying function. The `run/3` invocation is often easier to handle in programs. As with `ask/1`, you must supply your desired context.

## Fuse types

There are a couple of different fuse types in the system:

* Standard fuses, `{standard, MaxR, MaxT}`. These are fuses which tolerate `MaxR` melt attempts in a `MaxT` window, before they break down.
* Fault injection fuses, `{fault_injection, Rate, MaxR, MaxT}`. This fuse type sets up a fault injection scheme where the fuse fails at rate `Rate`, an floating point value between `0.0`–`1.0`. If you enter, say `1 / 500` then roughly every 500th request will se a `blown` fuse, even if the fuse is okay. This can be used to add noise to the system and verify that calling systems support the failure modes appropriately. The values `MaxR` and `MaxT` works as in a standard fuse.

## Administrative commands

An administrator can manually disable/reenable fuses through the following commands:

```erlang
ok = fuse:circuit_disable(Name),
…
ok = fuse:circuit_enable(Name),
```

When you disable a circuit, you blow the fuse until you enable the circuit again.

The interaction rules for disables/enables is that they dominate every other command except the call to `remove/1`. That is, even reinstalling an already installed fuse will not reenable it. The only way is to either call `fuse:circuit_enable/1` or by first `fuse:remove/1`'ing the fuse and then executing an `install/1` command.

## Monitoring fuse state

Fuses installed into the system are automatically instrumented in two ways: stats plugins and the `alarm_handler`.

### Stats plugins

Fuse includes a simple [behaviour](http://www.erlang.org/doc/design_principles/des_princ.html), `fuse_stats_plugin`, for integration with statistics and monitoring systems.

Independent of which stats plugin you use, the `ok` and `blown` metrics are increased on every `ask/2` call to the fuse. The `melt` metric is increased whenever we see a melt happen.

*Note:* The metrics are subject to change. Especially if someone can come up with better metrics to instrument for in the system.

* `fuse_stats_ets`: Maintains simple counts in an [ETS](http://www.erlang.org/doc/man/ets.html) table, which you can retrieve with a call to e.g. `fuse_stats_ets:counters(foo).`
* `fuse_stats_folsom`: Maintains [folsom](https://github.com/boundary/folsom) spirals `foo.ok`, `foo.blown` and `foo.melt`.
* `fuse_stats_exometer`: Maintains [exometer](https://github.com/Feuerlabs/exometer) spirals `[fuse, foo, ok]`, `[fuse, foo, blown]` and `[fuse, foo, melt]`.
* `fuse_stats_prometheus`: Maintains [prometheus](https://github.com/deadtrickster/prometheus.erl) counters `foo_responses_total[type="ok|blown"]` and `foo_melts_total`.

### Using a plugin

By default, fuse uses the `fuse_stats_ets` plugin. To use another, set it up in the environment with e.g.

```erlang
application:set_env(fuse, stats_plugin, fuse_stats_folsom).
```

or in a `.config` as

```erlang
{fuse, [ {stats_plugin, fuse_stats_folsom} ] }
```

Note that it's up to you to arrange your application's dependencies such that plugin applications like folsom or exometer are available and started. Fuse has no direct dependency on either folsom or exometer.

### Writing a plugin

See the source of `fuse_stats_plugin` and the plugins above for documentation and examples.

## Integration with the alarm handler

Furthermore, fuses raises alarms when they are blown. They raise an alarm under the same name as the fuse itself. To clear the alarm, the system uses hysteresis. It has to see 3 consecutive `ok` states on a fuse before clearing the alarm. This is to avoid alarm states from flapping excessively.

## Fuse events

The *fuse* system contains an event handler, `fuse_event` which can be used to listen on events and react when events trigger in the fuse subsystem. It will send events which are given by the following dialyzer specification `{atom(), blown | ok}`. Where the `atom()` is the name of the installed fuse.

The intended use is to evict waiters from queues in a system. Suppose you are queueing workers up for answers, blocking the workers in the process. When the workers were queued, the fuse was not blown, but now it suddenly broke. You can then install an event handler which pushes a message to the queueing process and tells it the fuse is broken. It can then react by evicting all the entries in queue as if the fuse was broken.

## Speed

### Standard fuses

On a Lenovo Thinkpad running Linux 3.14.4 with a processor `Intel(R) Core(TM) i7-3720QM CPU @ 2.60GHz` (An Ivy Bridge) Erlang Release 17.0.1, we get a throughput of 2.1 million fuse queries per second by running the stress test in `stress/stress.erl`. This test also has linear speedup over all the cores. Lookup times are sub-microsecond, usually around the 0.5 ballpark.

Running on a Q4 2013 Macbook Pro, OSX 10.9.2, 2 Ghz Intel Core i7 (Haswell) yields roughly the same speed.

In practice, your system will be doing other things as well, but do note that the overhead of enabling a fuse is expected to be around 0.5μs in overhead.

### Fault injecting fuses

A fuse running with fault injection has the added caveat that it also makes a call to `rand:uniform()` which in turn will slow down the request rate. It is not expected to be a lot of slowdown, but it is mentioned here for the sake of transparency.

## Tests

Fuse is written with two kinds of tests. First of all, it uses a set of Common Test test cases which runs the basic functionality of the system. Furthermore, fuse is written with Erlang QuickCheck test cases. EQC tests are written before the corresponding code is written, and as such, this is "Property Driven Development".

To run the standard tests, execute:

```bash
rebar3 ct
```

To run the EQC tests, you need a working EQC installation in your erlang path. Then I tend to do:

```bash
rebar3 shell
> cd("eqc_test").
> make:all().
> eqc_cluster:t(10).
```

I am deliberately keeping them out of the CI chain due to the necessity of Erlang Quickcheck in order to be able to run tests. There are a set of models, each testing one aspect of the fuse system. Taken together, they provide excellent coverage of the fuse system as a whole.

Great care has been taken in order to make sure fuse can be part of the error kernel of a system. The main fuse server is not supposed to crash under any circumstance. The monitoring application may crash since it is only part of the reporting. While important, it is not essential to correct operation.

## Requirements

QuickCheck allows us to test for requirements of a system. This essentially tests for any interleaving of calls toward the Fuse subsystem, so we weed out any error. We test for the following requirements as part of the test suite. We have verified that all of these requirements are being hit by typical EQC runs:

```text
Group heal:
R01 - Heal non-installed fuse (must never be triggered)
R02 - Heal installed fuse (only if blown already)

Group install:
R03 - Installation of a fuse with invalid configuation
R04 - Installation of a fuse with valid configuration

Group Reset:
R05 - Reset of an uninstalled fuse
R06 - Reset of an installed fuse (blown and nonblown)

Group Melt:
R11 - Melting of an installed fuse
R12 - Melting of an uninstalled fuse

Group run/2:
R07 - Use of run/2 on an ok fuse
R08 - Use of run/2 on a melted fuse
R09 - Use of run/2 on an ok fuse which is melted in the process
R10 - Use of run/2 on an uninstalled fus

Group blow:
R13 - Blowing a fuse
R14 - Removing melts from the window by expir

Group ask/1:
R15 - Ask on an installed fuse
R16 - Ask on an uninstalled fus

Group circuitry:
R17 - Disable an installed fuse
R18 - Disable an uninstalled fuse
R19 - Reenable an installed fuse
R20 - Reenable an uninstalled fuse
R21 - Melting a disabled fus

Group reset commands:
R22 - Heal command execution
R23 - Delay command execution
```

## EQC Test harness features

* Tests the fuse API in all cases with positive testing
* Uses negative testing to make sure the `install/2` command rejects wrong options correctly.
* Uses negative testing to make sure return values are correct for every other command
* Uses sequential testing to make sure command invocation is sane.
* Uses parallel testing to make sure there are no race conditions, even when many clients call into the system at the same time.
* Uses EQC PULSE to randomize the schedule of the processes we run to make sure they are correct.
* Models time in EQC and controls time advancement to test for situations where timing is a problem in the system under test.
* Uses EQC Component to monitor correct handling of alarms triggering and clearing in the Erlang system with correct hysteresis.

Furthermore:

* The EQC Test harness tests its internal consistency of time handling by using properties to test its own model for correctness.
* Makes a case as to why timestamping should happen inside `fuse_server` and not outside (clocks can skew and time can go backwards if clients draw from a time source. In a distributed setting, it is even worse).

## Subtle Errors found by EQC

Software construction is a subtle and elusive business. Most errors in software are weeded out early in the development cycle, the errors that remain are really rare and hard to find. Static type systems will only raise the bar and make the remaining errors even more slippery from your grasp. Thus, the only way to remove these errors is to use a tool which is good at construction elusive counter-examples. Erlang QuickCheck is such a tool.

Development guided by properties leads to a code base which is considerably smaller. In the course of building `fuse` we iteratively removed functionality from the code base which proved to be impossible to implement correctly. Rather than ending up with a lot of special cases, development by property simply suggested the removal of certain nasty configurations of the software, which does nothing but makes it more bloated. Worse—one could argue some of these configuration would never be used in practice, leading to written code which would never be traversed.

### General

* Numerous small mistakes have been weeded out while developing the code.
* EQC has guided the design in a positive way. This has lead to smaller and simpler code with fewer errors.
* EQC has suggested improvements to the API. Specifically in the area of synchronous calls and race condition avoidance.

### Subtleties

* If you `install/2` a fuse with an intensity of `0` it will start in the `blown` state and not in the `ok` state. The code did not account for this small detail.
* Parallel test case generation found a wrong reset invocation where the answer was `{error, no_such_fuse}` and not the specified `{error, not_found}`. Sequential tests did not find this particular interleaving problem. Subsequently, the discovery was an inadequacy in the sequential model with too weak pre-condition generation.
* More work on the model made it clear the fuse intensity of '0' requires much special handling in the code base and model to handle correctly. It was decided to reject an intensity of 0 altogether and shave off much complexity in the implementation and in the model.
* EQC and careful consideration came up with the idea to separate the alarm handling code from the fuse handling code in the system, to protect a faulty alarm handler from taking down the fuse system.
* EQC, using parallel testing, uncovered a problem with the synchronicity of `run/2`.
* EQC, and helpful hints by Thomas Arts, made it evident that the method used to draw timestamps was incorrect. A new model, where timestamps are generated inside the fuse system was much easier to test and verify.
* EQC made it clear exactly how time is used in the system.
* The EQC model suggested the introduction of the `Context` variable and the handling of different operating contexts. It also uncovered exactly how limited context handling leads to problems with lineariaziblity of the calls.
* Switching between fuse types had numerous errors, removed by EQC.
* The ability to administratively disable/enable fuses had weird interactions with operations which (re-)installed fuses into the system.
* Implementing a full component-based timing model found an off-by-one bug in period calculations.
* The component based model found situations where timers were set more than once.
* EQC found a case where timers are not correctly disabled when you call for circuit disables.
* EQC found an error when doing timer resets.

The monitor model found the following:

* Monitor handling was incorrect and the hysteresis was not implemented correctly, leading to numerous flaps on the alarms.
