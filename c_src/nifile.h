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

#ifndef NIFILE_H
#define NIFILE_H

#include <unistd.h>


#include "erl_nif.h"


#define ATOM_MAP(symname, defname, atomname) extern ERL_NIF_TERM ATOM_##defname;
#include "nifile_atoms.all"
#undef ATOM_MAP


void nifile_init_atoms(ErlNifEnv* env);

char* nifile_make_path(ErlNifBinary* path);

int nifile_open_oflags(ErlNifEnv* env, ERL_NIF_TERM oflags);
int nifile_open_modes(ErlNifEnv* env, ERL_NIF_TERM modes, int default_mode);

ERL_NIF_TERM nifile_errno_error(ErlNifEnv* env, int err);

ERL_NIF_TERM nifile_buffer(ErlNifEnv* env, ErlNifBinary* bin, ssize_t status);


#endif