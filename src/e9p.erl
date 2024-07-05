-module(e9p).

-export([]).

-export_type([qid/0, fid/0]).

-type qid() :: #{
                 type => integer(),
                 version => integer(),
                 path => integer()
                }.

-type fid() :: 16#00000000..16#FFFFFFFF.
