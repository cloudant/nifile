% Licensed under the Apache License, Version 2.0 (the "License"); you may not
% use this file except in compliance with the License. You may obtain a copy of
% the License at
%
%   http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
% License for the specific language governing permissions and limitations under
% the License.

-module(nifile_01_basic_tests).


-include_lib("eunit/include/eunit.hrl").


-define(FNAME, "foo.dat").
-define(DNAME, "foodir").
-define(ALPHABET, <<"abcdefghijklmnopqrstuvwxyz">>).


remove_file() ->
    case filelib:is_file(?FNAME) of
        true -> ok = file:delete(?FNAME);
        false -> ok
    end.


remove_dir() ->
    case filelib:is_dir(?DNAME) of
        true -> ok = file:del_dir(?DNAME);
        false -> ok
    end.


reset_file(Fd) ->
    {ok, 0} = nifile:seek(Fd, 0, seek_set),
    ok = nifile:truncate(Fd, 0).


open_close_test() ->
    remove_file(),

    ?assertEqual(false, filelib:is_file(?FNAME)),
    ?assertEqual({error, enoent}, nifile:open(?FNAME)),

    {ok, Fd} = nifile:open(?FNAME, [create, read_write]),

    ?assertEqual(true, filelib:is_file(?FNAME)),
    ?assertEqual(ok, nifile:close(Fd)),
    ?assertEqual({error, ebadf}, nifile:close(Fd)).


sync_test() ->
    remove_file(),

    {ok, Fd} = nifile:open(?FNAME, [create, read_write]),
    ?assertEqual(ok, nifile:sync(Fd)).


seek_test() ->
    remove_file(),

    {ok, Fd} = nifile:open(?FNAME, [create, read_write]),
    ?assertEqual({ok, 26}, nifile:write(Fd, ?ALPHABET)),

    ?assertEqual({ok, 0}, nifile:seek(Fd, 0, seek_set)),
    ?assertEqual({ok, <<"a">>}, nifile:read(Fd, 1)),

    ?assertEqual({ok, 13}, nifile:seek(Fd, 13, seek_set)),
    ?assertEqual({ok, <<"n">>}, nifile:read(Fd, 1)),

    ?assertEqual({ok, 27}, nifile:seek(Fd, 27, seek_set)),
    ?assertEqual({ok, <<>>}, nifile:read(Fd, 1)),

    {ok, 0} = nifile:seek(Fd, 0, seek_set),
    ?assertEqual({ok, 1}, nifile:seek(Fd, 1, seek_cur)),
    ?assertEqual({ok, 3}, nifile:seek(Fd, 2, seek_cur)),
    ?assertEqual({ok, 13}, nifile:seek(Fd, 10, seek_cur)),
    ?assertEqual({ok, 3}, nifile:seek(Fd, -10, seek_cur)),
    ?assertEqual({ok, 1}, nifile:seek(Fd, -2, seek_cur)),
    ?assertEqual({ok, <<"b">>}, nifile:read(Fd, 1)),

    ?assertEqual({ok, 26}, nifile:seek(Fd, 0, seek_end)),
    ?assertEqual({ok, 25}, nifile:seek(Fd, -1, seek_end)),
    ?assertEqual({ok, 28}, nifile:seek(Fd, 2, seek_end)),
    ?assertEqual({ok, 0}, nifile:seek(Fd, -26, seek_end)).



read_test() ->
    remove_file(),

    {ok, Fd} = nifile:open(?FNAME, [create, read_write]),
    {ok, 26} = nifile:write(Fd, ?ALPHABET),

    % At the end of the file, should return empty
    ?assertEqual({ok, <<>>}, nifile:read(Fd, 26)),

    % Read the whole file
    {ok, 0} = nifile:seek(Fd, 0, seek_set),
    ?assertEqual({ok, ?ALPHABET}, nifile:read(Fd, 26)),

    % Back to the end of the file
    ?assertEqual({ok, <<>>}, nifile:read(Fd, 26)),

    % Read one at a time
    {ok, 0} = nifile:seek(Fd, 0, seek_set),
    Bytes = lists:map(fun(_) ->
        {ok, C} = nifile:read(Fd, 1),
        C
    end, lists:seq(1, 26)),
    ?assertEqual(?ALPHABET, list_to_binary(Bytes)),

    % Read beyond the end of the file
    {ok, 0} = nifile:seek(Fd, 0, seek_set),
    ?assertEqual({ok, ?ALPHABET}, nifile:read(Fd, 52)),

    % Can read zero bytes
    {ok, 0} = nifile:seek(Fd, 0, seek_set),
    ?assertEqual({ok, <<>>}, nifile:read(Fd, 0)),

    % Read beyond the file from the middle of the file
    {ok, 13} = nifile:seek(Fd, 13, seek_set),
    ?assertEqual({ok, <<"nopqrstuvwxyz">>}, nifile:read(Fd, 52)).


write_test() ->
    remove_file(),

    {ok, Fd} = nifile:open(?FNAME, [create, read_write]),

    ?assertEqual({ok, 26}, nifile:write(Fd, ?ALPHABET)),

    {ok, 0} = nifile:seek(Fd, 0, seek_set),
    ?assertEqual({ok, ?ALPHABET}, nifile:read(Fd, 26)),

    ?assertEqual({ok, 26}, nifile:write(Fd, ?ALPHABET)),
    {ok, 0} = nifile:seek(Fd, 0, seek_set),
    TwoAlphabets = list_to_binary([?ALPHABET, ?ALPHABET]),
    ?assertEqual({ok, TwoAlphabets}, nifile:read(Fd, 52)),

    reset_file(Fd),

    lists:foreach(fun(C) ->
        ?assertEqual({ok, 1}, nifile:write(Fd, [C]))
    end, binary_to_list(?ALPHABET)),
    {ok, 0} = nifile:seek(Fd, 0, seek_set),
    ?assertEqual({ok, ?ALPHABET}, nifile:read(Fd, 26)).


append_test() ->
    remove_file(),

    {ok, Fd} = nifile:open(?FNAME, [create, read_write, append]),

    ?assertEqual({ok, 26}, nifile:write(Fd, ?ALPHABET)),

    ?assertEqual({ok, ?ALPHABET}, nifile:pread(Fd, 26, 0)),

    {ok, 0} = nifile:seek(Fd, 0, seek_set),

    ?assertEqual({ok, 26}, nifile:write(Fd, ?ALPHABET)),

    TwoAlphabets = list_to_binary([?ALPHABET, ?ALPHABET]),
    ?assertEqual({ok, TwoAlphabets}, nifile:pread(Fd, 52, 0)),

    lists:foreach(fun(C) ->
        ?assertEqual({ok, 1}, nifile:write(Fd, [C]))
    end, binary_to_list(?ALPHABET)),

    ThreeAlphabets = list_to_binary([?ALPHABET, ?ALPHABET, ?ALPHABET]),
    ?assertEqual({ok, ThreeAlphabets}, nifile:pread(Fd, 78, 0)).


pread_test() ->
    remove_file(),

    {ok, Fd} = nifile:open(?FNAME, [create, read_write]),
    {ok, 26} = nifile:write(Fd, ?ALPHABET),

    % At the end of the file, should return empty
    ?assertEqual({ok, <<>>}, nifile:pread(Fd, 26, 26)),

    % Read the whole file
    ?assertEqual({ok, ?ALPHABET}, nifile:pread(Fd, 26, 0)),

    % Read one at a time
    {ok, 0} = nifile:seek(Fd, 0, seek_set),
    Bytes = lists:map(fun(Pos) ->
        {ok, C} = nifile:pread(Fd, 1, Pos),
        C
    end, lists:seq(0, 25)),
    ?assertEqual(?ALPHABET, list_to_binary(Bytes)),

    % Read beyond the end of the file
    ?assertEqual({ok, ?ALPHABET}, nifile:pread(Fd, 52, 0)),

    % Can read zero bytes
    ?assertEqual({ok, <<>>}, nifile:pread(Fd, 0, 0)),

    % Read beyond the file from the middle of the file
    ?assertEqual({ok, <<"nopqrstuvwxyz">>}, nifile:pread(Fd, 52, 13)).


pwrite_test() ->
    remove_file(),

    {ok, Fd} = nifile:open(?FNAME, [create, read_write]),

    ?assertEqual({ok, 26}, nifile:pwrite(Fd, ?ALPHABET, 0)),

    ?assertEqual({ok, ?ALPHABET}, nifile:pread(Fd, 26, 0)),

    ?assertEqual({ok, 26}, nifile:pwrite(Fd, ?ALPHABET, 26)),
    TwoAlphabets = list_to_binary([?ALPHABET, ?ALPHABET]),
    ?assertEqual({ok, TwoAlphabets}, nifile:pread(Fd, 52, 0)),

    reset_file(Fd),

    lists:foreach(fun({Pos, Byte}) ->
        ?assertEqual({ok, 1}, nifile:pwrite(Fd, [Byte], Pos))
    end, lists:zip(lists:seq(0, 25), binary_to_list(?ALPHABET))),
    ?assertEqual({ok, ?ALPHABET}, nifile:pread(Fd, 26, 0)).


truncate_test() ->
    remove_file(),

    {ok, Fd} = nifile:open(?FNAME, [create, read_write]),
    {ok, 26} = nifile:write(Fd, ?ALPHABET),

    lists:foreach(fun(Len) ->
        ?assertEqual(ok, nifile:truncate(Fd, Len)),
        ?assertEqual({ok, Len}, nifile:seek(Fd, 0, seek_end))
    end, lists:reverse(lists:seq(0, 25))),

    ?assertEqual({ok, <<>>}, nifile:pread(Fd, 1, 0)),

    % Truncate to extend
    ?assertEqual(ok, nifile:truncate(Fd, 5)),
    ?assertEqual({ok, <<0, 0, 0, 0, 0>>}, nifile:pread(Fd, 10, 0)).


rename_test() ->
    remove_file(),

    {ok, Fd} = nifile:open(?FNAME, [create, read_write]),
    ok = nifile:close(Fd),

    ?assertEqual(ok, nifile:rename(?FNAME, "bar.dat")),
    ?assertEqual(true, filelib:is_file("bar.dat")).


mkdir_test() ->
    remove_dir(),

    ?assertEqual(ok, nifile:mkdir(?DNAME)),
    ?assertEqual(true, filelib:is_dir(?DNAME)).


rmdir_test() ->
    remove_dir(),

    ok = nifile:mkdir(?DNAME),

    ?assertEqual(true, filelib:is_dir(?DNAME)),
    ?assertEqual(ok, nifile:rmdir(?DNAME)),
    ?assertEqual(false, filelib:is_dir(?DNAME)).


unlink_test() ->
    remove_file(),

    {ok, Fd} = nifile:open(?FNAME, [create, read_write]),
    ok = nifile:close(Fd),

    ?assertEqual(true, filelib:is_file(?FNAME)),
    ?assertEqual(ok, nifile:unlink(?FNAME)),
    ?assertEqual(false, filelib:is_file(?FNAME)).


remove_test() ->
    remove_file(),
    remove_dir(),

    {ok, Fd} = nifile:open(?FNAME, [create, read_write]),
    ok = nifile:close(Fd),
    ?assertEqual(true, filelib:is_file(?FNAME)),
    ?assertEqual(ok, nifile:remove(?FNAME)),
    ?assertEqual(false, filelib:is_file(?FNAME)),

    ok = nifile:mkdir(?DNAME),
    ?assertEqual(true, filelib:is_dir(?DNAME)),
    ?assertEqual(ok, nifile:remove(?DNAME)),
    ?assertEqual(false, filelib:is_dir(?DNAME)).


lsdir_test() ->
    {ok, EntryStrs} = file:list_dir_all("."),
    Entries = [list_to_binary(ES) || ES <- EntryStrs],
    AllEntries = lists:sort([<<".">>, <<"..">> | Entries]),
    {ok, NifileEntries} = nifile:lsdir("."),
    ?assertEqual(AllEntries, lists:sort(NifileEntries)).

