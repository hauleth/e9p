% SPDX-FileCopyrightText: 2026 Łukasz Niemier <~@hauleth.dev>
%
% SPDX-License-Identifier: Apache-2.0

-module(e9p_sysfs_SUITE).

-compile(export_all).

-include_lib("stdlib/include/assert.hrl").
-include_lib("common_test/include/ct.hrl").

all() -> [
          can_list_mount_content,
          current_process_is_listed,
          current_process_current_function,
          system_info_atom_count,
          applications_list,
          application_info,
          application_env
         ].

init_per_suite(Config) ->
    PrivDir = ?config(priv_dir, Config),
    Path = filename:join(PrivDir, "sysfs"),
    Port = 19999,
    ok = file:make_dir(Path),
    {ok, PID} = e9p_server:start(Port, {e9p_sysfs, []}),
    ct:pal(Path),
    Cmd = io_lib:format("9pfs -p ~B localhost ~s",
                        [Port, Path]),
    ct:pal(Cmd),
    _Out = os:cmd(Cmd, #{ exception_on_failure => true }),
    [{sysfs, PID}, {mount, Path} | Config].

end_per_suite(Config) ->
    PID = ?config(sysfs, Config),
    Mount = ?config(mount, Config),
    os:cmd(["umount ", Mount], #{exception_on_failure => true}),
    erlang:exit(PID, normal),
    Config.

can_list_mount_content(Config) ->
    Mount = ?config(mount, Config),
    ct:pal(Mount),
    ?assertEqual([
                  "applications",
                  "processes",
                  "system_info"
                 ], ls(Mount)).

current_process_is_listed(Config) ->
    Mount = ?config(mount, Config),
    Path = filename:join([Mount, "processes", pid_to_list(self())]),
    ?assertEqual([
                  "current_function",
                  "dictionary",
                  "error_handler",
                  "garbage_collection",
                  "group_leader",
                  "heap_size",
                  "initial_call",
                  "links",
                  "message_queue_len",
                  "priority",
                  "reductions",
                  "stack_size",
                  "status",
                  "total_heap_size",
                  "trap_exit"
                 ], ls(Path)).

current_process_current_function(Config) ->
    Mount = ?config(mount, Config),
    Path = filename:join([
                          Mount,
                          "processes",
                          pid_to_list(self()),
                          "current_function"
                         ]),
    {ok, Bin} = file:read_file(Path),
    ?assertEqual(~"{gen,do_call,4}", Bin).

system_info_atom_count(Config) ->
    Mount = ?config(mount, Config),
    Path = filename:join([
                          Mount,
                          "system_info",
                          "atom_count"
                         ]),
    {ok, Bin} = file:read_file(Path),
    ?assertEqual(erlang:system_info(atom_count), binary_to_integer(Bin)).

applications_list(Config) ->
    Mount = ?config(mount, Config),
    Path = filename:join([
                          Mount,
                          "applications"
                         ]),
    List = ls(Path),
    Apps = lists:sort(lists:map(fun({AppName, _, _}) -> atom_to_list(AppName) end,
                                application:loaded_applications())),
    ?assertEqual(Apps, List).

application_info(Config) ->
    Mount = ?config(mount, Config),
    Path = filename:join([
                          Mount,
                          "applications",
                          "kernel"
                         ]),
    List = ls(Path),
    ?assertEqual([
                  "applications",
                  "description",
                  "env",
                  "id",
                  "included_applications",
                  "maxP",
                  "maxT",
                  "mod",
                  "modules",
                  "optional_applications",
                  "registered",
                  "start_phases",
                  "vsn"
                 ], List).

application_env(Config) ->
    Mount = ?config(mount, Config),
    Path = filename:join([
                          Mount,
                          "applications",
                          "kernel",
                          "env"
                         ]),
    {ok, [Read]} = file:consult(Path),
    Env = application:get_all_env(kernel),
    ?assertEqual(Read, Env).

%% Helpers

ls(Path) ->
    {ok, Files} = file:list_dir(Path),
    % Remove `.fscache` added on macOS
    lists:sort(Files) -- [".fscache"].
