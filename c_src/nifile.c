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


#include <dirent.h>
#include <errno.h>
#include <fcntl.h>
#include <stdio.h>
#include <string.h>
#include <sys/stat.h>
#include <unistd.h>


#include "nifile.h"

#define NIFILE_SCHEDULER_TYPE 0
//#define NIFILE_SCHEDULER_TYPE ERL_NIF_DIRTY_JOB_IO_BOUND


ErlNifResourceType* FD_RES;
ErlNifResourceType* DIR_RES;


typedef struct {
    int fd;
    int closed;
} nifile_fd_t;


typedef struct {
    DIR* dp;
    int closed;
} nifile_dir_t;


static ERL_NIF_TERM
nifile_fd_alloc(ErlNifEnv* env, int fd)
{
    nifile_fd_t* res;
    ERL_NIF_TERM ret;

    res = enif_alloc_resource(FD_RES, sizeof(nifile_fd_t));
    if(res == NULL) {
        return enif_make_badarg(env);
    }

    ret = enif_make_resource(env, res);
    enif_release_resource(res);

    res->fd = fd;
    res->closed = 0;

    return enif_make_tuple2(env, ATOM_OK, ret);
}


static void
nifile_fd_free(ErlNifEnv* env, void* obj)
{
    nifile_fd_t* res = (nifile_fd_t*) obj;
    if(!res->closed) {
        close(res->fd);
    }
}


static ERL_NIF_TERM
nifile_dir_alloc(ErlNifEnv* env, DIR* dp)
{
    nifile_dir_t* res;
    ERL_NIF_TERM ret;

    res = enif_alloc_resource(DIR_RES, sizeof(nifile_dir_t));
    if(res == NULL) {
        return enif_make_badarg(env);
    }

    ret = enif_make_resource(env, res);
    enif_release_resource(res);

    res->dp = dp;
    res->closed = 0;

    return enif_make_tuple2(env, ATOM_OK, ret);
}


static void
nifile_dir_free(ErlNifEnv* env, void* obj)
{
    nifile_dir_t* res = (nifile_dir_t*) obj;
    if(!res->closed) {
        closedir(res->dp);
    }
}


static inline nifile_fd_t*
nifile_get_fd_res(ErlNifEnv* env, ERL_NIF_TERM res) {
    void* obj;

    if(!enif_get_resource(env, res, FD_RES, &obj))
    {
	    return NULL;
    }

    return (nifile_fd_t*) obj;
}


static inline nifile_dir_t*
nifile_get_dir_res(ErlNifEnv* env, ERL_NIF_TERM res) {
    void* obj;

    if(!enif_get_resource(env, res, DIR_RES, &obj))
    {
	    return NULL;
    }

    return (nifile_dir_t*) obj;
}


static ERL_NIF_TERM
nifile_open(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ErlNifBinary path_bin;
    char* path = NULL;
    int fd = -1;
    int oflags = 0;
    int modes = 0;

    if(!enif_inspect_iolist_as_binary(env, argv[0], &path_bin)) {
        return enif_make_badarg(env);
    }

    oflags = nifile_open_oflags(env, argv[1]);
    if(oflags < 0) {
        return enif_make_badarg(env);
    }

    modes = nifile_open_modes(env, argv[2], S_IRUSR | S_IWUSR);
    if(modes < 0) {
        return enif_make_badarg(env);
    }

    path = nifile_make_path(&path_bin);
    if(path == NULL) {
        return enif_make_tuple2(env, ATOM_ERROR, ATOM_ENOMEM);
    }

    fd = open(path, oflags, modes);

    enif_free(path);

    if(fd < 0) {
        return nifile_errno_error(env, errno);
    }

    return nifile_fd_alloc(env, fd);
}


static ERL_NIF_TERM
nifile_close(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    nifile_fd_t* res = nifile_get_fd_res(env, argv[0]);
    int ret;

    if(res == NULL) {
        return enif_make_badarg(env);
    }

    if(res->fd <= 0) {
        return enif_make_badarg(env);
    }

    ret = close(res->fd);

    if(ret == 0) {
        res->closed = 1;
        return ATOM_OK;
    }

    return nifile_errno_error(env, errno);
}


static ERL_NIF_TERM
nifile_sync(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    nifile_fd_t* res = nifile_get_fd_res(env, argv[0]);

    if(res == NULL) {
        return enif_make_badarg(env);
    }

    if(fsync(res->fd) != 0) {
        return nifile_errno_error(env, errno);
    }

    return ATOM_OK;
}


static ERL_NIF_TERM
nifile_seek(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    nifile_fd_t* res = nifile_get_fd_res(env, argv[0]);
    off_t offset;
    int whence;
    off_t ret;

    if(res == NULL) {
        return enif_make_badarg(env);
    }

    if(!enif_get_int64(env, argv[1], (ErlNifSInt64*) &offset)) {
        return enif_make_badarg(env);
    }

    if(argv[2] == ATOM_SEEK_SET) {
        whence = SEEK_SET;
    } else if(argv[2] == ATOM_SEEK_CUR) {
        whence = SEEK_CUR;
    } else if(argv[2] == ATOM_SEEK_END) {
        whence = SEEK_END;
    } else {
        return enif_make_badarg(env);
    }

    ret = lseek(res->fd, offset, whence);

    if(ret < 0) {
        return nifile_errno_error(env, errno);
    }

    return enif_make_tuple2(env, ATOM_OK, enif_make_int64(env, ret));
}


static ERL_NIF_TERM
nifile_read(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    nifile_fd_t* res = nifile_get_fd_res(env, argv[0]);
    size_t count;
    ErlNifBinary bin;
    ssize_t status;

    if(res == NULL) {
        return enif_make_badarg(env);
    }

    if(!enif_get_uint64(env, argv[1], (ErlNifUInt64*) &count)) {
        return enif_make_badarg(env);
    }

    if(!enif_alloc_binary(count, &bin)) {
        return enif_make_tuple2(env, ATOM_ERROR, ATOM_ENOMEM);
    }

    status = read(res->fd, bin.data, count);

    return nifile_buffer(env, &bin, status);
}


static ERL_NIF_TERM
nifile_write(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    nifile_fd_t* res = nifile_get_fd_res(env, argv[0]);
    ErlNifBinary bin;
    ssize_t status;

    if(res == NULL) {
        return enif_make_badarg(env);
    }

    if(!enif_inspect_iolist_as_binary(env, argv[1], &bin)) {
        return enif_make_badarg(env);
    }

    status = write(res->fd, bin.data, bin.size);

    if(status < 0) {
        return nifile_errno_error(env, errno);
    } else {
        return enif_make_tuple2(env, ATOM_OK, enif_make_int64(env, status));
    }
}


static ERL_NIF_TERM
nifile_pread(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    nifile_fd_t* res = nifile_get_fd_res(env, argv[0]);
    size_t count;
    off_t offset;
    ErlNifBinary bin;
    ssize_t status;

    if(res == NULL) {
        return enif_make_badarg(env);
    }

    if(!enif_get_uint64(env, argv[1], (ErlNifUInt64*) &count)) {
        return enif_make_badarg(env);
    }

    if(!enif_get_int64(env, argv[2], (ErlNifSInt64*) &offset)) {
        return enif_make_badarg(env);
    }

    if(!enif_alloc_binary(count, &bin)) {
        return enif_make_tuple2(env, ATOM_ERROR, ATOM_ENOMEM);
    }

    status = pread(res->fd, bin.data, count, offset);

    return nifile_buffer(env, &bin, status);
}


static ERL_NIF_TERM
nifile_pwrite(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    nifile_fd_t* res = nifile_get_fd_res(env, argv[0]);
    ErlNifBinary bin;
    off_t offset;
    ssize_t status;

    if(res == NULL) {
        return enif_make_badarg(env);
    }

    if(!enif_inspect_iolist_as_binary(env, argv[1], &bin)) {
        return enif_make_badarg(env);
    }

    if(!enif_get_uint64(env, argv[2], (ErlNifUInt64*) &offset)) {
        return enif_make_badarg(env);
    }

    status = pwrite(res->fd, bin.data, bin.size, offset);

    if(status < 0) {
        return nifile_errno_error(env, errno);
    } else {
        return enif_make_tuple2(env, ATOM_OK, enif_make_int64(env, status));
    }
}


static ERL_NIF_TERM
nifile_truncate(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    nifile_fd_t* res = nifile_get_fd_res(env, argv[0]);
    off_t offset;
    int ret;

    if(res == NULL) {
        return enif_make_badarg(env);
    }

    if(!enif_get_int64(env, argv[1], (ErlNifSInt64*) &offset)) {
        return enif_make_badarg(env);
    }

    ret = ftruncate(res->fd, offset);

    if(ret == 0) {
        return ATOM_OK;
    }

    return nifile_errno_error(env, errno);
}


static ERL_NIF_TERM
nifile_rename(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ErlNifBinary src_bin;
    ErlNifBinary tgt_bin;
    char* src_path = NULL;
    char* tgt_path = NULL;
    int ret;

    if(!enif_inspect_iolist_as_binary(env, argv[0], &src_bin)) {
        return enif_make_badarg(env);
    }

    if(!enif_inspect_iolist_as_binary(env, argv[1], &tgt_bin)) {
        return enif_make_badarg(env);
    }

    src_path = nifile_make_path(&src_bin);
    if(src_path == NULL) {
        return enif_make_tuple2(env, ATOM_ERROR, ATOM_ENOMEM);
    }

    tgt_path = nifile_make_path(&tgt_bin);
    if(tgt_path == NULL) {
        enif_free(src_path);
        return enif_make_tuple2(env, ATOM_ERROR, ATOM_ENOMEM);
    }

    ret = rename(src_path, tgt_path);

    enif_free(src_path);
    enif_free(tgt_path);

    if(ret == 0) {
        return ATOM_OK;
    }

    return nifile_errno_error(env, errno);
}


static ERL_NIF_TERM
nifile_mkdir(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ErlNifBinary path_bin;
    char* path = NULL;
    int modes = 0;
    int ret;

    if(!enif_inspect_iolist_as_binary(env, argv[0], &path_bin)) {
        return enif_make_badarg(env);
    }

    modes = nifile_open_modes(env, argv[1], S_IRWXU);
    if(modes < 0) {
        return enif_make_badarg(env);
    }

    path = nifile_make_path(&path_bin);
    if(path == NULL) {
        return enif_make_tuple2(env, ATOM_ERROR, ATOM_ENOMEM);
    }

    ret = mkdir(path, modes);

    enif_free(path);

    if(ret == 0) {
        return ATOM_OK;
    }

    return nifile_errno_error(env, errno);
}


static ERL_NIF_TERM
nifile_rmdir(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ErlNifBinary path_bin;
    char* path = NULL;
    int ret;

    if(!enif_inspect_iolist_as_binary(env, argv[0], &path_bin)) {
        return enif_make_badarg(env);
    }

    path = nifile_make_path(&path_bin);
    if(path == NULL) {
        return enif_make_tuple2(env, ATOM_ERROR, ATOM_ENOMEM);
    }

    ret = rmdir(path);

    enif_free(path);

    if(ret == 0) {
        return ATOM_OK;
    }

    return nifile_errno_error(env, errno);
}


static ERL_NIF_TERM
nifile_unlink(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ErlNifBinary path_bin;
    char* path = NULL;
    int ret;

    if(!enif_inspect_iolist_as_binary(env, argv[0], &path_bin)) {
        return enif_make_badarg(env);
    }

    path = nifile_make_path(&path_bin);
    if(path == NULL) {
        return enif_make_tuple2(env, ATOM_ERROR, ATOM_ENOMEM);
    }

    ret = unlink(path);

    enif_free(path);

    if(ret == 0) {
        return ATOM_OK;
    }

    return nifile_errno_error(env, errno);
}


static ERL_NIF_TERM
nifile_remove(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ErlNifBinary path_bin;
    char* path = NULL;
    int ret;

    if(!enif_inspect_iolist_as_binary(env, argv[0], &path_bin)) {
        return enif_make_badarg(env);
    }

    path = nifile_make_path(&path_bin);
    if(path == NULL) {
        return enif_make_tuple2(env, ATOM_ERROR, ATOM_ENOMEM);
    }

    ret = remove(path);

    enif_free(path);

    if(ret == 0) {
        return ATOM_OK;
    }

    return nifile_errno_error(env, errno);
}


static ERL_NIF_TERM
nifile_opendir(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ErlNifBinary path_bin;
    char* path = NULL;
    DIR* dp;

    if(!enif_inspect_iolist_as_binary(env, argv[0], &path_bin)) {
        return enif_make_badarg(env);
    }

    path = nifile_make_path(&path_bin);
    if(path == NULL) {
        return enif_make_tuple2(env, ATOM_ERROR, ATOM_ENOMEM);
    }

    dp = opendir(path);

    enif_free(path);

    if(dp == NULL) {
        return nifile_errno_error(env, errno);
    }

    return nifile_dir_alloc(env, dp);
}


static ERL_NIF_TERM
nifile_closedir(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    nifile_dir_t* res = nifile_get_dir_res(env, argv[0]);
    int ret;

    if(res == NULL) {
        return enif_make_badarg(env);
    }

    ret = closedir(res->dp);

    if(ret == 0) {
        res->closed = 1;
        return ATOM_OK;
    }

    return nifile_errno_error(env, errno);
}


static ERL_NIF_TERM
nifile_readdir(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    nifile_dir_t* res = nifile_get_dir_res(env, argv[0]);
    struct dirent entry;
    struct dirent* result;
    int ret;
    ErlNifBinary bin;

    if(res == NULL) {
        return enif_make_badarg(env);
    }

    ret = readdir_r(res->dp, &entry, &result);

    if(ret != 0) {
        return nifile_errno_error(env, errno);
    }

    if(result == NULL) {
        return ATOM_EOD;
    }

    if(!enif_alloc_binary(strlen(result->d_name), &bin)) {
        return enif_make_tuple2(env, ATOM_ERROR, ATOM_ENOMEM);
    }

    memcpy(bin.data, result->d_name, bin.size);

    return enif_make_tuple2(env, ATOM_OK, enif_make_binary(env, &bin));
}


static int
nifile_load(ErlNifEnv* env, void** priv, ERL_NIF_TERM load_info)
{
    ErlNifSysInfo sys_info;
    enif_system_info(&sys_info, sizeof(ErlNifSysInfo));
    if(!sys_info.smp_support || !sys_info.dirty_scheduler_support) {
        return 1;
    }

    nifile_init_atoms(env);

    int flags = ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER;

    FD_RES = enif_open_resource_type(
            env, NULL, "nifile_fd_t", nifile_fd_free, flags, NULL);

    if(FD_RES == NULL) {
        return -1;
    }

    DIR_RES = enif_open_resource_type(
            env, NULL, "nifile_dir_t", nifile_dir_free, flags, NULL);

    if(DIR_RES == NULL) {
        return -1;
    }

    return 0;
}


static ErlNifFunc nif_funcs[] = {
    {"open",        3, nifile_open,         NIFILE_SCHEDULER_TYPE},
    {"close",       1, nifile_close,        NIFILE_SCHEDULER_TYPE},
    {"sync",        1, nifile_sync,         NIFILE_SCHEDULER_TYPE},
    {"seek",        3, nifile_seek,         NIFILE_SCHEDULER_TYPE},
    {"read",        2, nifile_read,         NIFILE_SCHEDULER_TYPE},
    {"write",       2, nifile_write,        NIFILE_SCHEDULER_TYPE},
    {"pread",       3, nifile_pread,        NIFILE_SCHEDULER_TYPE},
    {"pwrite",      3, nifile_pwrite,       NIFILE_SCHEDULER_TYPE},
    {"truncate",    2,  nifile_truncate,    NIFILE_SCHEDULER_TYPE},

    {"rename",      2, nifile_rename,       NIFILE_SCHEDULER_TYPE},
    {"mkdir",       2, nifile_mkdir,        NIFILE_SCHEDULER_TYPE},
    {"rmdir",       1, nifile_rmdir,        NIFILE_SCHEDULER_TYPE},
    {"unlink",      1, nifile_unlink,       NIFILE_SCHEDULER_TYPE},
    {"remove",      1, nifile_remove,       NIFILE_SCHEDULER_TYPE},

    {"opendir",     1, nifile_opendir,      NIFILE_SCHEDULER_TYPE},
    {"closedir",    1, nifile_closedir,     NIFILE_SCHEDULER_TYPE},
    {"readdir",     1, nifile_readdir,      NIFILE_SCHEDULER_TYPE}
};


ERL_NIF_INIT(nifile, nif_funcs, &nifile_load, NULL, NULL, NULL);
