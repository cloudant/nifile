// Licensed under the Apache License, Version 2.0 (the "License"); you may not
// use this file except in compliance with the License. You may obtain a copy of
// the License at
//
//   http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
// WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
// License for the specific language governing permissions and limitations under
// the License.

#include <stdio.h>

#include <errno.h>
#include <fcntl.h>
#include <string.h>

#include "nifile.h"


char*
nifile_make_path(ErlNifBinary* path)
{
    char* ret = (char*) enif_alloc(path->size + 1);

    if(ret == NULL) {
        return NULL;
    }

    memcpy(ret, path->data, path->size);
    ret[path->size] = 0;

    return ret;
}


#define ATOM_MAP(symname, defname, atomname) \
    if(oflag == ATOM_##defname) {ret |= symname;}

int
nifile_open_oflags(ErlNifEnv* env, ERL_NIF_TERM oflags)
{
    ERL_NIF_TERM oflag;
    int ret = 0;

    if(!enif_is_list(env, oflags)) {
        return -1;
    }

    while(enif_get_list_cell(env, oflags, &oflag, &oflags)) {
#include "nifile_atoms.oflags"
    }

    return ret;
}

#undef ATOM_MAP


#define ATOM_MAP(symname, defname, atomname) \
    if(mode == ATOM_##defname) {ret |= symname;}

int
nifile_open_modes(ErlNifEnv* env, ERL_NIF_TERM modes, int default_mode)
{
    ERL_NIF_TERM mode;
    int ret = 0;

    if(!enif_is_list(env, modes)) {
        return -1;
    }

    if(enif_is_empty_list(env, modes)) {
        return default_mode;
    }

    while(enif_get_list_cell(env, modes, &mode, &modes)) {
#include "nifile_atoms.modes"
    }

    return ret;
}

#undef ATOM_MAP


#define ATOM_MAP(symname, defname, atomname) \
    if(err == symname) {reason = ATOM_##defname;}

ERL_NIF_TERM
nifile_errno_error(ErlNifEnv* env, int err)
{
    ERL_NIF_TERM reason = ATOM_UNKNOWN_ERROR;

#include "nifile_atoms.errno"

    return enif_make_tuple2(env, ATOM_ERROR, reason);
}

#undef ATOM_MAP


ERL_NIF_TERM
nifile_buffer(ErlNifEnv* env, ErlNifBinary* bin, ssize_t status)
{
    ErlNifBinary tmp;
    int err;

    if(status < 0) {
        // In case enif_release_binary affects errno we
        // capture it here first.
        err = errno;
        enif_release_binary(bin);
        return nifile_errno_error(env, err);
    } else if(status < bin->size) {
        if(!enif_alloc_binary((size_t) status, &tmp)) {
            enif_release_binary(bin);
            return enif_make_tuple2(env, ATOM_ERROR, ATOM_ENOMEM);
        }

        memcpy(tmp.data, bin->data, tmp.size);
        enif_release_binary(bin);

        // tmp is owned by the return
        // value so we don't release it.
        return enif_make_tuple2(env, ATOM_OK, enif_make_binary(env, &tmp));
    } else {
        // bin is owned by the return value
        // so we don't release it.
        return enif_make_tuple2(env, ATOM_OK, enif_make_binary(env, bin));
    }
}

