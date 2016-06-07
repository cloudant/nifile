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

-module(nifile).
-on_load(init/0).

-export([
    open/1,
    open/2,
    open/3,
    close/1,
    fd/1,
    sync/1,
    seek/3,
    read/2,
    write/2,
    pread/3,
    pwrite/3,
    truncate/2,

    rename/2,
    mkdir/1,
    mkdir/2,
    rmdir/1,
    unlink/1,
    remove/1,

    lsdir/1,
    lsdir/3,

    % Advanced API instead of lsdir/1,3
    opendir/1,
    closedir/1,
    readdir/1
]).


-define(NOT_LOADED,
        erlang:nif_error({not_loaded, [{module, ?MODULE}, {line, ?LINE}]})).


open(Path) ->
    open(Path, [], []).


open(Path, OpenFlags) ->
    open(Path, OpenFlags, []).


open(_Path, _OpenFlags, _Modes) ->
    ?NOT_LOADED.


close(_Fd) ->
    ?NOT_LOADED.


fd(_Fd) ->
    ?NOT_LOADED.


sync(_Fd) ->
    ?NOT_LOADED.


seek(_Fd, _Offset, _Whence) ->
    ?NOT_LOADED.


read(_Fd, _Size) ->
    ?NOT_LOADED.


write(_Fd, _IoData) ->
    ?NOT_LOADED.


pread(_Fd, _Size, _Offset) ->
    ?NOT_LOADED.


pwrite(_Fd, _IoData, _Offset) ->
    ?NOT_LOADED.


truncate(_Fd, _Size) ->
    ?NOT_LOADED.


rename(_SrcPath, _TgtPath) ->
    ?NOT_LOADED.


mkdir(Path) ->
    mkdir(Path, []).


mkdir(_Path, _cModes) ->
    ?NOT_LOADED.


rmdir(_Path) ->
    ?NOT_LOADED.


unlink(_Path) ->
    ?NOT_LOADED.


remove(_Path) ->
    ?NOT_LOADED.


lsdir(Path) ->
    lsdir(Path, fun(P, Acc) -> [P | Acc] end, []).


lsdir(Path, Fun, Acc) ->
    case opendir(Path) of
        {ok, Dp} ->
            try
                lsdir_loop(Dp, Fun, Acc)
            after
                closedir(Dp)
            end;
        Else ->
            Else
    end.


lsdir_loop(Dp, Fun, Acc) ->
    case readdir(Dp) of
        {ok, Path} ->
            NewAcc = Fun(Path, Acc),
            lsdir_loop(Dp, Fun, NewAcc);
        eod ->
            {ok, Acc};
        Else ->
            Else
    end.


opendir(_Path) ->
    ?NOT_LOADED.


closedir(_Dp) ->
    ?NOT_LOADED.


readdir(_Dp) ->
    ?NOT_LOADED.


init() ->
    SoName = case code:priv_dir(?MODULE) of
        {error, bad_name} ->
            case filelib:is_dir(filename:join(["..", priv])) of
                true ->
                    filename:join(["..", priv, ?MODULE]);
                _ ->
                    filename:join([priv, ?MODULE])
            end;
        Dir ->
            filename:join(Dir, ?MODULE)
    end,
    erlang:load_nif(SoName, 0).
