-module(fuse_cluster).

-include_lib("eqc/include/eqc.hrl").
-include_lib("eqc/include/eqc_cluster.hrl").

-compile(export_all).

components() -> [
	fuse_time_eqc,
	fuse_eqc
].

api_spec() -> api_spec(?MODULE).

prop_cluster_correct() ->
    ?SETUP(fun() ->
        eqc_mocking:start_mocking(api_spec(), components()),
        fuse_eqc:setup(),
        fun() -> ok end
    end,
    ?FORALL(Cmds, eqc_cluster:commands(?MODULE),
       begin
          cleanup(),
          {H,S,R} = eqc_cluster:run_commands(?MODULE, Cmds),
          pretty_commands(?MODULE, Cmds, {H,S,R},
            aggregate(with_title('Commands'), command_names(Cmds),
            aggregate(with_title('Features'), eqc_statem:call_features(H),
            features(eqc_statem:call_features(H),
                R == ok))))
      end))).
          