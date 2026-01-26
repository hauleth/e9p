-module(e9p_unfs_SUITE).

-compile(export_all).

-include_lib("stdlib/include/assert.hrl").
-include_lib("common_test/include/ct.hrl").

all() -> [
          list_mount,
          read_file
          %% Ignore this test for now, as UID/GID translation is not possible
          %% right now until we implement 9p2000.u extension
          % write_to_existing_file
         ].

init_per_suite(Config) ->
    PrivDir = ?config(priv_dir, Config),
    Source = filename:join(PrivDir, "h"),
    Mount = filename:join(PrivDir, "c"),
    Port = 6666,
    ok = file:make_dir(Source),
    ok = file:make_dir(Mount),
    {ok, PID} = e9p_server:start(Port, {e9p_unfs, #{path => Source}}),
    ct:pal("source: ~s", [Source]),
    ct:pal("mount: ~s", [Mount]),
    Cmd = io_lib:format("9pfs -p ~B localhost ~p",
                        [Port, Mount]),
    ct:pal(Cmd),
    _Out = os:cmd(Cmd, #{ exception_on_failure => true }),
    [{fs, PID}, {mount, Mount}, {source, Source} | Config].

end_per_suite(Config) ->
    PID = ?config(fs, Config),
    Mount = ?config(mount, Config),
    os:cmd(["umount ", Mount], #{exception_on_failure => true}),
    erlang:exit(PID, normal),
    Config.

list_mount(Config) ->
    Source = ?config(source, Config),
    Mount = ?config(mount, Config),
    file:write_file([Source, "/bar"], "example data"),
    ct:pal(Mount),
    ?assertEqual(["bar"], ls(Mount)).

read_file(Config) ->
    Source = ?config(source, Config),
    Mount = ?config(mount, Config),
    file:write_file([Source, "/bar"], "example data"),
    ct:pal(Mount),
    ?assertEqual({ok, ~"example data"}, file:read_file([Mount, "/bar"])).

write_to_existing_file(Config) ->
    Source = ?config(source, Config),
    Mount = ?config(mount, Config),
    file:write_file([Source, "/bar"], ""),
    ct:pal(Mount),
    ok = file:write_file([Mount, "/bar"], "another data"),
    ?assertEqual({ok, ~"another data"}, file:read_file([Source, "/bar"])).

%% Helpers

ls(Path) ->
    {ok, Files} = file:list_dir(Path),
    % Remove `.fscache` added on macOS
    lists:sort(Files) -- [".fscache"].
