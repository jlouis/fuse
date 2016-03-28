-module(fuse_cluster).

-ifdef(EQC_TESTING).

-include_lib("eqc/include/eqc.hrl").
-include_lib("eqc/include/eqc_cluster.hrl").

-compile(export_all).

components() -> [
	fuse_time_eqc,
	fuse_eqc
].

api_spec() -> eqc_cluster:api_spec(?MODULE).

prop_cluster_correct() ->
    ?SETUP(fun() ->
        eqc_mocking:start_mocking(api_spec(), components()),
        fuse_eqc:setup(),
        fun() -> ok end
    end,
    ?FORALL(Cmds, eqc_statem:more_commands(3, eqc_cluster:commands(?MODULE)),
       begin
          fuse_eqc:cleanup(),
          {H,S,R} = eqc_cluster:run_commands(?MODULE, Cmds),
          pretty_commands(?MODULE, Cmds, {H,S,R},
            aggregate(with_title('Commands'), command_names(Cmds),
            collect(eqc_lib:summary('Length'), length(Cmds),
            aggregate(with_title('Features'), eqc_statem:call_features(H),
            features(eqc_statem:call_features(H),
                R == ok)))))
      end)).

t() -> t(15).

t(Secs) ->
    eqc:quickcheck(eqc:testing_time(Secs, eqc_statem:show_states(prop_cluster_correct()))).
    
recheck() ->
    eqc:recheck(eqc_statem:show_states(prop_cluster_correct())).
    
-endif.
