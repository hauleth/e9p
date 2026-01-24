-module(e9p_sysfs).

-behaviour(e9p_fs).

-include_lib("kernel/include/logger.hrl").

-export([init/1,
         root/3,
         walk/4,
         open/4,
         create/6,
         read/5,
         write/5,
         remove/3,
         stat/3,
         wstat/4]).

init(_State) -> {ok, []}.

root(_Uname, _Aname, State) ->
    {ok, #{qid := QID}} = stat_for([]),
    {ok, {QID, []}, State}.

walk(_FID, Path, Name, State) ->
    ?LOG_DEBUG(#{path => Path, name => Name}),
    case stat_for(Path ++ [Name]) of
        {ok, #{qid := QID}} ->
            {{QID, []}, State};
        {error, Reason} ->
            {error, Reason, State}
    end.

open(_FID, [], _Mode, State) ->
    {ok, {[~"applications", ~"processes", ~"system_info"], 0}, State};
open(_FID, [~"system_info"], _Mode, State) ->
    Keys = [
            ~"allocated_areas",
            % ~"allocator", %% TBD
            ~"alloc_util_allocators",
            % ~"allocator_sizes", %% TBD
            ~"cpu_topology",
            ~"logical_processors",
            ~"logical_processors_available",
            ~"logical_processors_online",
            ~"cpu_quota",
            ~"update_cpu_info",
            ~"fullsweep_after",
            ~"garbage_collection",
            ~"heap_sizes",
            ~"heap_type",
            ~"max_heap_size",
            ~"message_queue_data",
            ~"min_heap_size",
            ~"min_bin_vheap_size",
            ~"procs",
            ~"atom_count",
            ~"atom_limit",
            ~"ets_count",
            ~"ets_limit",
            ~"port_count",
            ~"port_limit",
            ~"process_count",
            ~"process_limit",
            ~"end_time",
            ~"os_monotonic_time_source",
            ~"os_system_time_source",
            ~"start_time",
            ~"time_correction",
            ~"time_offset",
            ~"time_warp_mode",
            ~"tolerant_timeofday",
            ~"dirty_cpu_schedulers",
            ~"dirty_cpu_schedulers_online",
            ~"dirty_io_schedulers",
            ~"multi_scheduling",
            ~"multi_scheduling_blockers",
            ~"normal_multi_scheduling_blockers",
            ~"scheduler_bind_type",
            ~"scheduler_bindings",
            % ~"scheduler_id", %% Intentionally omitted
            ~"schedulers",
            ~"schedulers_online",
            ~"smp_support",
            ~"threads",
            ~"thread_pool_size",
            ~"async_dist",
            ~"creation",
            ~"delayed_node_table_gc",
            ~"dist",
            ~"dist_buf_busy_limit",
            ~"dist_ctrl",
            ~"c_compiler_used",
            ~"check_io",
            ~"debug_compiled",
            ~"driver_version",
            ~"dynamic_trace",
            ~"dynamic_trace_probes",
            ~"emu_flavor",
            ~"emu_type",
            ~"halt_flush_timeout",
            ~"info",
            ~"kernel_poll",
            ~"loaded",
            ~"machine",
            ~"modified_timing_level",
            ~"nif_version",
            ~"otp_release",
            ~"outstanding_system_requests_limit",
            ~"port_parallelism",
            ~"system_architecture",
            ~"system_logger",
            ~"system_version",
            ~"trace_control_word",
            ~"version",
            ~"wordsize"
           ],
    {ok, {Keys, 0}, State};
open(_FID, [~"system_info", ~"wordsize"], _Mode, State) ->
    {ok, {[~"internal", ~"external"], 0}, State};
open(_FID, [~"system_info", ~"wordsize", TypeB], _Mode, State) ->
    Type = binary_to_atom(TypeB),
    Wordsize = erlang:system_info({wordsize, Type}),
    Data = iolist_to_binary(io_lib:format("~B", [Wordsize])),
    {ok, {Data, 0}, State};
open(_FID, [~"system_info", KeyB], _Mode, State) ->
    Key = binary_to_atom(KeyB),
    Data = case erlang:system_info(Key) of
               Val when is_binary(Val) -> Val;
               Val ->
                   case io_lib:printable_list(Val) of
                       true -> unicode:characters_to_binary(Val);
                       false -> iolist_to_binary(io_lib:format("~p", [Val]))
                   end
           end,
    {ok, {Data, 0}, State};
%% ===== Processes =====
open(_FID, [~"processes"], _Mode, State) ->
    Processes = lists:map(fun pid_to_list/1, erlang:processes()),
    {ok, {Processes, 0}, State};
open(_FID, [~"processes", _PID], _Mode, State) ->
    Keys = [
            ~"current_function",
            ~"initial_call",
            ~"status",
            ~"message_queue_len",
            ~"links",
            ~"dictionary",
            ~"trap_exit",
            ~"error_handler",
            ~"priority",
            ~"group_leader",
            ~"total_heap_size",
            ~"heap_size",
            ~"stack_size",
            ~"reductions",
            ~"garbage_collection"
           ],
    {ok, {Keys, 0}, State};
open(_FID, [~"processes", PIDB, KeyB], _Mode, State) ->
    PIDL = binary_to_list(PIDB),
    PID = list_to_pid(PIDL),
    Key = binary_to_existing_atom(KeyB),
    case erlang:process_info(PID, Key) of
        {Key, Val} ->
            Data = iolist_to_binary(io_lib:format("~p", [Val])),
            {ok, {Data, 0}, State};
        [] -> {ok, {[], 0}, State};
        undefined ->
            {error, "No such file", State}
    end;
%% ===== Applications =====
open(_FID, [~"applications"], _Mode, State) ->
    AllApps = lists:map(fun({Name, _, _}) when is_atom(Name) -> erlang:atom_to_binary(Name) end,
                        application:loaded_applications()),
    {ok, {AllApps, 0}, State};
open(_FID, [~"applications", Name], _Mode, State) ->
    Atom = binary_to_existing_atom(Name),
    {ok, AppKeys} = application:get_all_key(Atom),
    Keys = proplists:get_keys(AppKeys),
    Files = lists:map(fun erlang:atom_to_binary/1, Keys),
    {ok, {Files, 0}, State};
open(_FID, [~"applications", Name, ~"env"], _Mode, State) ->
    Atom = binary_to_existing_atom(Name),
    AllEnv = application:get_all_env(Atom),
    Data = iolist_to_binary(io_lib:format("~p", [AllEnv])),
    {ok, {Data, 0}, State};
open(_FID, [~"applications", NameB, KeyB], _Mode, State) ->
    Name = binary_to_existing_atom(NameB),
    Key = binary_to_existing_atom(KeyB),
    {ok, Val} = application:get_key(Name, Key),
    Data = iolist_to_binary(io_lib:format("~p", [Val])),
    {ok, {Data, 0}, State}.

create(_FID, _Path, _Name, _Perm, _Mode, State) ->
    {error, "Not supported", State}.

read({QID, Data}, Path, Offset, Length, State) ->
    case e9p:is_type(QID, directory) of
        true -> readdir(Data, Path, Offset, Length, State);
        false -> readfile(Data, Path, Offset, Length, State)
    end.

readdir(Data, Path, Offset, Length, State) ->
    Encoded = lists:map(fun(Entry) ->
                            {ok, Stat} = stat_for(Path ++ [Entry]),
                            e9p_msg:encode_stat(Stat)
                    end, Data),
    Bin = iolist_to_binary(Encoded),
    {ok, {Data, chunk(Bin, Offset, Length)}, State}.

readfile(Data, _Path, Offset, Length, State) when is_integer(Length) ->
    {ok, {Data, chunk(Data, Offset, Length)}, State}.

chunk(Data, Offset, Length)
  when is_binary(Data), is_integer(Offset), is_integer(Length) ->
    if
        Offset =< byte_size(Data) ->
            Len = min(Length, byte_size(Data) - Offset),
            binary_part(Data, Offset, Len);
        true -> ~""
    end.

write(_FID, _Path, _Offset, _Data, State) ->
    {error, "Unimplemented", State}.

remove(_FID, _Path, State) ->
    {error, "Unimplemented", State}.

stat(_FID, Path, State) ->
    case stat_for(Path) of
        {ok, Stat} -> {ok, Stat, State};
        {error, Reason} -> {error, Reason, State}
    end.

wstat(_FID, _Path, _Stat, State) ->
    {error, "Not supported", State}.

stat_for([]) ->
    {ok, #{
      qid => e9p:make_qid(directory, 0, 0),
      name => ~"/",
      mode => 8#555,
      length => 0
     }};
stat_for([~"processes"]) ->
    {ok, #{
           qid => e9p:make_qid(directory, 0, 1),
           name => ~"processes",
           mode => 8#555,
           length => 0
          }};
stat_for([~"processes", PID]) ->
    Hash = erlang:phash2(PID, 16#FFFF),
    <<Value:64>> = <<16#1, 0:24, Hash:32>>,
    {ok, #{
           qid => e9p:make_qid(directory, 0, Value),
           name => PID, 
           mode => 8#555,
           length => 0
          }};
stat_for([~"processes", PID, Key]) ->
    Hash = erlang:phash2(PID, 16#FFFF),
    KeyH = erlang:phash2(Key, 16#FFF),
    <<Value:64>> = <<16#1, KeyH:24, Hash:32>>,
    {ok, #{
           qid => e9p:make_qid(regular, 0, Value),
           name => Key, 
           mode => 8#555,
           length => 0
          }};
stat_for([~"applications"]) ->
    {ok, #{
           qid => e9p:make_qid(directory, 0, 2),
           name => ~"applications",
           mode => 8#555,
           length => 0
          }};
stat_for([~"applications", Name]) ->
    Atom = binary_to_existing_atom(Name),
    case application:get_key(Atom, vsn) of
        undefined -> {error, "Not exist"};
        {ok, _Vsn} ->
            Hash = erlang:phash2(Name, 16#FFFF),
            <<Value:64>> = <<16#2, 0:24, Hash:32>>,
            {ok,
             #{
               qid => e9p:make_qid(directory, 0, Value),
               name => Name,
               mode => 8#555,
               length => 0
              }}
    end;
stat_for([~"applications", Name, Key]) ->
    Hash = erlang:phash2(Name, 16#FFFF),
    KeyH = erlang:phash2(Key, 16#FFF),
    <<Value:64>> = <<16#2, KeyH:24, Hash:32>>,
    {ok, #{
           qid => e9p:make_qid(regular, 0, Value),
           name => Key,
           mode => 8#444,
           length => 0
          }};
stat_for([~"system_info"]) ->
    {ok, #{
           qid => e9p:make_qid(directory, 0, 3),
           name => ~"system_info",
           mode => 8#555,
           length => 0
          }};
stat_for([~"system_info", ~"wordsize"]) ->
    Hash = erlang:phash2(~"wordsize", 16#FFFF),
    <<Value:64>> = <<16#2, 0:24, Hash:32>>,
    {ok, #{
           qid => e9p:make_qid(directory, 0, Value),
           name => ~"wordsize",
           mode => 8#555,
           length => 0
          }};
stat_for([~"system_info", ~"wordsize", ~"internal"]) ->
    Hash = erlang:phash2(~"wordsize", 16#FFFF),
    <<Value:64>> = <<16#2, 1:24, Hash:32>>,
    {ok, #{
           qid => e9p:make_qid(regular, 0, Value),
           name => ~"internal",
           mode => 8#444,
           length => 0
          }};
stat_for([~"system_info", ~"wordsize", ~"external"]) ->
    Hash = erlang:phash2(~"wordsize", 16#FFFF),
    <<Value:64>> = <<16#2, 2:24, Hash:32>>,
    {ok, #{
           qid => e9p:make_qid(regular, 0, Value),
           name => ~"external",
           mode => 8#444,
           length => 0
          }};
stat_for([~"system_info", Name]) ->
    Hash = erlang:phash2(Name, 16#FFFF),
    <<Value:64>> = <<16#2, 0:24, Hash:32>>,
    {ok, #{
           qid => e9p:make_qid(regular, 0, Value),
           name => Name,
           mode => 8#444,
           length => 0
          }};
stat_for(_Path) ->
    {error, "Not exist"}.
