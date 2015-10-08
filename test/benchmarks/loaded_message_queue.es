#!/usr/bin/env escript

-mode(compile).

file_size() -> 10485760. % 10MiB
chunk_size() -> 4096.


script_dir() ->
    filename:dirname(escript:script_name()).


file_name() ->
    filename:join(script_dir(), "loaded_message_queues.tmp").


set_code_path() ->
    EbinDir = filename:join([script_dir(), "..", "..", "ebin"]),
    code:add_patha(EbinDir).


erlang() ->
    {
        "Erlang file module",
        fun(FName) -> file:open(FName, [read, append, raw, binary]) end,
        fun(Fd) -> file:close(Fd) end,
        fun(Fd, Binary) -> file:write(Fd, Binary) end,
        fun(Fd, Pos, Len) -> file:pread(Fd, Pos, Len) end
    }.


nifile() ->
    {
        "nifile",
        fun(FName) -> nifile:open(FName, [create, read_write, append]) end,
        fun(Fd) -> nifile:close(Fd) end,
        fun(Fd, Binary) -> {ok, _} = nifile:write(Fd, Binary), ok end,
        fun(Fd, Pos, Len) -> nifile:pread(Fd, Len, Pos) end
    }.


main(_) ->
    set_code_path(),
    Impls = [nifile(), erlang()],
    MessageCounts = [0, 10, 1000, 10000, 100000, 1000000],
    lists:foreach(fun(Impl) ->
        {Description, _, _, _, _} = Impl,
        io:format("~s:~n", [Description]),
        lists:foreach(fun(M) ->
            run_test(Impl, M, file_size())
        end, MessageCounts),
        io:format("~n", [])
    end, Impls),
    ok.


run_test(TestFuns, Messages, Bytes) ->
    Self = self(),
    {Pid, _} = spawn_monitor(fun() ->
        run_loop(Self, TestFuns, Messages, Bytes)
    end),
    lists:foreach(fun(_) ->
        Pid ! ignored_message
    end, lists:seq(1, Messages)),
    Pid ! go,
    receive {'DOWN', _, _, Pid, _} -> ok end.


run_loop(_Parent, TestFuns, Messages, Bytes) ->
    receive go -> ok end,
    case filelib:is_file(file_name()) of
        true -> file:delete(file_name());
        _ -> ok
    end,
    {_, Open, Close, Write, Read} = TestFuns,
    {ok, Fd} = Open(file_name()),
    {WriteTime, ReadTime} = try
        {WT, _} = timer:tc(fun() -> write_loop(Write, Fd, Bytes) end),
        {RT, _} = timer:tc(fun() -> read_loop(Read, Fd, Bytes, 0) end),
        {WT, RT}
    after
        Close(Fd)
    end,
    Args = [
        Messages,
        WriteTime / 1000000,
        ReadTime / 1000000,
        (WriteTime + ReadTime) / 1000000
    ],
    io:format("~8b :: Write: ~8.2fs  Read: ~8.2fs Total: ~8.2fs~n", Args).


write_loop(_Write, _Fd, Bytes) when Bytes =< 0 ->
    ok;
write_loop(Write, Fd, Bytes) ->
    Bin = crypto:rand_bytes(chunk_size()),
    ok = Write(Fd, Bin),
    write_loop(Write, Fd, Bytes-chunk_size()).

read_loop(_Read, _Fd, Bytes, Pos) when Pos >= Bytes ->
    ok;
read_loop(Read, Fd, Bytes, Pos) ->
    {ok, _Bin} = Read(Fd, Pos, chunk_size()),
    read_loop(Read, Fd, Bytes, Pos+chunk_size()).

