/*
 * Please do not edit this file.
 * It was generated using rpcgen.
 */

#ifndef _GUESTFS_PROTOCOL_H_RPCGEN
#define _GUESTFS_PROTOCOL_H_RPCGEN

#include <rpc/rpc.h>


#ifdef __cplusplus
extern "C" {
#endif


typedef char *str;

struct guestfs_lvm_int_pv {
	char *pv_name;
	char pv_uuid[32];
	char *pv_fmt;
	quad_t pv_size;
	quad_t dev_size;
	quad_t pv_free;
	quad_t pv_used;
	char *pv_attr;
	quad_t pv_pe_count;
	quad_t pv_pe_alloc_count;
	char *pv_tags;
	quad_t pe_start;
	quad_t pv_mda_count;
	quad_t pv_mda_free;
};
typedef struct guestfs_lvm_int_pv guestfs_lvm_int_pv;

typedef struct {
	u_int guestfs_lvm_int_pv_list_len;
	guestfs_lvm_int_pv *guestfs_lvm_int_pv_list_val;
} guestfs_lvm_int_pv_list;

struct guestfs_lvm_int_vg {
	char *vg_name;
	char vg_uuid[32];
	char *vg_fmt;
	char *vg_attr;
	quad_t vg_size;
	quad_t vg_free;
	char *vg_sysid;
	quad_t vg_extent_size;
	quad_t vg_extent_count;
	quad_t vg_free_count;
	quad_t max_lv;
	quad_t max_pv;
	quad_t pv_count;
	quad_t lv_count;
	quad_t snap_count;
	quad_t vg_seqno;
	char *vg_tags;
	quad_t vg_mda_count;
	quad_t vg_mda_free;
};
typedef struct guestfs_lvm_int_vg guestfs_lvm_int_vg;

typedef struct {
	u_int guestfs_lvm_int_vg_list_len;
	guestfs_lvm_int_vg *guestfs_lvm_int_vg_list_val;
} guestfs_lvm_int_vg_list;

struct guestfs_lvm_int_lv {
	char *lv_name;
	char lv_uuid[32];
	char *lv_attr;
	quad_t lv_major;
	quad_t lv_minor;
	quad_t lv_kernel_major;
	quad_t lv_kernel_minor;
	quad_t lv_size;
	quad_t seg_count;
	char *origin;
	float snap_percent;
	float copy_percent;
	char *move_pv;
	char *lv_tags;
	char *mirror_log;
	char *modules;
};
typedef struct guestfs_lvm_int_lv guestfs_lvm_int_lv;

typedef struct {
	u_int guestfs_lvm_int_lv_list_len;
	guestfs_lvm_int_lv *guestfs_lvm_int_lv_list_val;
} guestfs_lvm_int_lv_list;

struct guestfs_int_stat {
	quad_t dev;
	quad_t ino;
	quad_t mode;
	quad_t nlink;
	quad_t uid;
	quad_t gid;
	quad_t rdev;
	quad_t size;
	quad_t blksize;
	quad_t blocks;
	quad_t atime;
	quad_t mtime;
	quad_t ctime;
};
typedef struct guestfs_int_stat guestfs_int_stat;

struct guestfs_int_statvfs {
	quad_t bsize;
	quad_t frsize;
	quad_t blocks;
	quad_t bfree;
	quad_t bavail;
	quad_t files;
	quad_t ffree;
	quad_t favail;
	quad_t fsid;
	quad_t flag;
	quad_t namemax;
};
typedef struct guestfs_int_statvfs guestfs_int_statvfs;

struct guestfs_mount_args {
	char *device;
	char *mountpoint;
};
typedef struct guestfs_mount_args guestfs_mount_args;

struct guestfs_touch_args {
	char *path;
};
typedef struct guestfs_touch_args guestfs_touch_args;

struct guestfs_cat_args {
	char *path;
};
typedef struct guestfs_cat_args guestfs_cat_args;

struct guestfs_cat_ret {
	char *content;
};
typedef struct guestfs_cat_ret guestfs_cat_ret;

struct guestfs_ll_args {
	char *directory;
};
typedef struct guestfs_ll_args guestfs_ll_args;

struct guestfs_ll_ret {
	char *listing;
};
typedef struct guestfs_ll_ret guestfs_ll_ret;

struct guestfs_ls_args {
	char *directory;
};
typedef struct guestfs_ls_args guestfs_ls_args;

struct guestfs_ls_ret {
	struct {
		u_int listing_len;
		str *listing_val;
	} listing;
};
typedef struct guestfs_ls_ret guestfs_ls_ret;

struct guestfs_list_devices_ret {
	struct {
		u_int devices_len;
		str *devices_val;
	} devices;
};
typedef struct guestfs_list_devices_ret guestfs_list_devices_ret;

struct guestfs_list_partitions_ret {
	struct {
		u_int partitions_len;
		str *partitions_val;
	} partitions;
};
typedef struct guestfs_list_partitions_ret guestfs_list_partitions_ret;

struct guestfs_pvs_ret {
	struct {
		u_int physvols_len;
		str *physvols_val;
	} physvols;
};
typedef struct guestfs_pvs_ret guestfs_pvs_ret;

struct guestfs_vgs_ret {
	struct {
		u_int volgroups_len;
		str *volgroups_val;
	} volgroups;
};
typedef struct guestfs_vgs_ret guestfs_vgs_ret;

struct guestfs_lvs_ret {
	struct {
		u_int logvols_len;
		str *logvols_val;
	} logvols;
};
typedef struct guestfs_lvs_ret guestfs_lvs_ret;

struct guestfs_pvs_full_ret {
	guestfs_lvm_int_pv_list physvols;
};
typedef struct guestfs_pvs_full_ret guestfs_pvs_full_ret;

struct guestfs_vgs_full_ret {
	guestfs_lvm_int_vg_list volgroups;
};
typedef struct guestfs_vgs_full_ret guestfs_vgs_full_ret;

struct guestfs_lvs_full_ret {
	guestfs_lvm_int_lv_list logvols;
};
typedef struct guestfs_lvs_full_ret guestfs_lvs_full_ret;

struct guestfs_read_lines_args {
	char *path;
};
typedef struct guestfs_read_lines_args guestfs_read_lines_args;

struct guestfs_read_lines_ret {
	struct {
		u_int lines_len;
		str *lines_val;
	} lines;
};
typedef struct guestfs_read_lines_ret guestfs_read_lines_ret;

struct guestfs_aug_init_args {
	char *root;
	int flags;
};
typedef struct guestfs_aug_init_args guestfs_aug_init_args;

struct guestfs_aug_defvar_args {
	char *name;
	str *expr;
};
typedef struct guestfs_aug_defvar_args guestfs_aug_defvar_args;

struct guestfs_aug_defvar_ret {
	int nrnodes;
};
typedef struct guestfs_aug_defvar_ret guestfs_aug_defvar_ret;

struct guestfs_aug_defnode_args {
	char *name;
	char *expr;
	char *val;
};
typedef struct guestfs_aug_defnode_args guestfs_aug_defnode_args;

struct guestfs_aug_defnode_ret {
	int nrnodes;
	bool_t created;
};
typedef struct guestfs_aug_defnode_ret guestfs_aug_defnode_ret;

struct guestfs_aug_get_args {
	char *path;
};
typedef struct guestfs_aug_get_args guestfs_aug_get_args;

struct guestfs_aug_get_ret {
	char *val;
};
typedef struct guestfs_aug_get_ret guestfs_aug_get_ret;

struct guestfs_aug_set_args {
	char *path;
	char *val;
};
typedef struct guestfs_aug_set_args guestfs_aug_set_args;

struct guestfs_aug_insert_args {
	char *path;
	char *label;
	bool_t before;
};
typedef struct guestfs_aug_insert_args guestfs_aug_insert_args;

struct guestfs_aug_rm_args {
	char *path;
};
typedef struct guestfs_aug_rm_args guestfs_aug_rm_args;

struct guestfs_aug_rm_ret {
	int nrnodes;
};
typedef struct guestfs_aug_rm_ret guestfs_aug_rm_ret;

struct guestfs_aug_mv_args {
	char *src;
	char *dest;
};
typedef struct guestfs_aug_mv_args guestfs_aug_mv_args;

struct guestfs_aug_match_args {
	char *path;
};
typedef struct guestfs_aug_match_args guestfs_aug_match_args;

struct guestfs_aug_match_ret {
	struct {
		u_int matches_len;
		str *matches_val;
	} matches;
};
typedef struct guestfs_aug_match_ret guestfs_aug_match_ret;

struct guestfs_aug_ls_args {
	char *path;
};
typedef struct guestfs_aug_ls_args guestfs_aug_ls_args;

struct guestfs_aug_ls_ret {
	struct {
		u_int matches_len;
		str *matches_val;
	} matches;
};
typedef struct guestfs_aug_ls_ret guestfs_aug_ls_ret;

struct guestfs_rm_args {
	char *path;
};
typedef struct guestfs_rm_args guestfs_rm_args;

struct guestfs_rmdir_args {
	char *path;
};
typedef struct guestfs_rmdir_args guestfs_rmdir_args;

struct guestfs_rm_rf_args {
	char *path;
};
typedef struct guestfs_rm_rf_args guestfs_rm_rf_args;

struct guestfs_mkdir_args {
	char *path;
};
typedef struct guestfs_mkdir_args guestfs_mkdir_args;

struct guestfs_mkdir_p_args {
	char *path;
};
typedef struct guestfs_mkdir_p_args guestfs_mkdir_p_args;

struct guestfs_chmod_args {
	int mode;
	char *path;
};
typedef struct guestfs_chmod_args guestfs_chmod_args;

struct guestfs_chown_args {
	int owner;
	int group;
	char *path;
};
typedef struct guestfs_chown_args guestfs_chown_args;

struct guestfs_exists_args {
	char *path;
};
typedef struct guestfs_exists_args guestfs_exists_args;

struct guestfs_exists_ret {
	bool_t existsflag;
};
typedef struct guestfs_exists_ret guestfs_exists_ret;

struct guestfs_is_file_args {
	char *path;
};
typedef struct guestfs_is_file_args guestfs_is_file_args;

struct guestfs_is_file_ret {
	bool_t fileflag;
};
typedef struct guestfs_is_file_ret guestfs_is_file_ret;

struct guestfs_is_dir_args {
	char *path;
};
typedef struct guestfs_is_dir_args guestfs_is_dir_args;

struct guestfs_is_dir_ret {
	bool_t dirflag;
};
typedef struct guestfs_is_dir_ret guestfs_is_dir_ret;

struct guestfs_pvcreate_args {
	char *device;
};
typedef struct guestfs_pvcreate_args guestfs_pvcreate_args;

struct guestfs_vgcreate_args {
	char *volgroup;
	struct {
		u_int physvols_len;
		str *physvols_val;
	} physvols;
};
typedef struct guestfs_vgcreate_args guestfs_vgcreate_args;

struct guestfs_lvcreate_args {
	char *logvol;
	char *volgroup;
	int mbytes;
};
typedef struct guestfs_lvcreate_args guestfs_lvcreate_args;

struct guestfs_mkfs_args {
	char *fstype;
	char *device;
};
typedef struct guestfs_mkfs_args guestfs_mkfs_args;

struct guestfs_sfdisk_args {
	char *device;
	int cyls;
	int heads;
	int sectors;
	struct {
		u_int lines_len;
		str *lines_val;
	} lines;
};
typedef struct guestfs_sfdisk_args guestfs_sfdisk_args;

struct guestfs_write_file_args {
	char *path;
	char *content;
	int size;
};
typedef struct guestfs_write_file_args guestfs_write_file_args;

struct guestfs_umount_args {
	char *pathordevice;
};
typedef struct guestfs_umount_args guestfs_umount_args;

struct guestfs_mounts_ret {
	struct {
		u_int devices_len;
		str *devices_val;
	} devices;
};
typedef struct guestfs_mounts_ret guestfs_mounts_ret;

struct guestfs_file_args {
	char *path;
};
typedef struct guestfs_file_args guestfs_file_args;

struct guestfs_file_ret {
	char *description;
};
typedef struct guestfs_file_ret guestfs_file_ret;

struct guestfs_command_args {
	struct {
		u_int arguments_len;
		str *arguments_val;
	} arguments;
};
typedef struct guestfs_command_args guestfs_command_args;

struct guestfs_command_ret {
	char *output;
};
typedef struct guestfs_command_ret guestfs_command_ret;

struct guestfs_command_lines_args {
	struct {
		u_int arguments_len;
		str *arguments_val;
	} arguments;
};
typedef struct guestfs_command_lines_args guestfs_command_lines_args;

struct guestfs_command_lines_ret {
	struct {
		u_int lines_len;
		str *lines_val;
	} lines;
};
typedef struct guestfs_command_lines_ret guestfs_command_lines_ret;

struct guestfs_stat_args {
	char *path;
};
typedef struct guestfs_stat_args guestfs_stat_args;

struct guestfs_stat_ret {
	guestfs_int_stat statbuf;
};
typedef struct guestfs_stat_ret guestfs_stat_ret;

struct guestfs_lstat_args {
	char *path;
};
typedef struct guestfs_lstat_args guestfs_lstat_args;

struct guestfs_lstat_ret {
	guestfs_int_stat statbuf;
};
typedef struct guestfs_lstat_ret guestfs_lstat_ret;

struct guestfs_statvfs_args {
	char *path;
};
typedef struct guestfs_statvfs_args guestfs_statvfs_args;

struct guestfs_statvfs_ret {
	guestfs_int_statvfs statbuf;
};
typedef struct guestfs_statvfs_ret guestfs_statvfs_ret;

struct guestfs_tune2fs_l_args {
	char *device;
};
typedef struct guestfs_tune2fs_l_args guestfs_tune2fs_l_args;

struct guestfs_tune2fs_l_ret {
	struct {
		u_int superblock_len;
		str *superblock_val;
	} superblock;
};
typedef struct guestfs_tune2fs_l_ret guestfs_tune2fs_l_ret;

struct guestfs_blockdev_setro_args {
	char *device;
};
typedef struct guestfs_blockdev_setro_args guestfs_blockdev_setro_args;

struct guestfs_blockdev_setrw_args {
	char *device;
};
typedef struct guestfs_blockdev_setrw_args guestfs_blockdev_setrw_args;

struct guestfs_blockdev_getro_args {
	char *device;
};
typedef struct guestfs_blockdev_getro_args guestfs_blockdev_getro_args;

struct guestfs_blockdev_getro_ret {
	bool_t ro;
};
typedef struct guestfs_blockdev_getro_ret guestfs_blockdev_getro_ret;

struct guestfs_blockdev_getss_args {
	char *device;
};
typedef struct guestfs_blockdev_getss_args guestfs_blockdev_getss_args;

struct guestfs_blockdev_getss_ret {
	int sectorsize;
};
typedef struct guestfs_blockdev_getss_ret guestfs_blockdev_getss_ret;

struct guestfs_blockdev_getbsz_args {
	char *device;
};
typedef struct guestfs_blockdev_getbsz_args guestfs_blockdev_getbsz_args;

struct guestfs_blockdev_getbsz_ret {
	int blocksize;
};
typedef struct guestfs_blockdev_getbsz_ret guestfs_blockdev_getbsz_ret;

struct guestfs_blockdev_setbsz_args {
	char *device;
	int blocksize;
};
typedef struct guestfs_blockdev_setbsz_args guestfs_blockdev_setbsz_args;

struct guestfs_blockdev_getsz_args {
	char *device;
};
typedef struct guestfs_blockdev_getsz_args guestfs_blockdev_getsz_args;

struct guestfs_blockdev_getsz_ret {
	quad_t sizeinsectors;
};
typedef struct guestfs_blockdev_getsz_ret guestfs_blockdev_getsz_ret;

struct guestfs_blockdev_getsize64_args {
	char *device;
};
typedef struct guestfs_blockdev_getsize64_args guestfs_blockdev_getsize64_args;

struct guestfs_blockdev_getsize64_ret {
	quad_t sizeinbytes;
};
typedef struct guestfs_blockdev_getsize64_ret guestfs_blockdev_getsize64_ret;

struct guestfs_blockdev_flushbufs_args {
	char *device;
};
typedef struct guestfs_blockdev_flushbufs_args guestfs_blockdev_flushbufs_args;

struct guestfs_blockdev_rereadpt_args {
	char *device;
};
typedef struct guestfs_blockdev_rereadpt_args guestfs_blockdev_rereadpt_args;

struct guestfs_upload_args {
	char *remotefilename;
};
typedef struct guestfs_upload_args guestfs_upload_args;

struct guestfs_download_args {
	char *remotefilename;
};
typedef struct guestfs_download_args guestfs_download_args;

struct guestfs_checksum_args {
	char *csumtype;
	char *path;
};
typedef struct guestfs_checksum_args guestfs_checksum_args;

struct guestfs_checksum_ret {
	char *checksum;
};
typedef struct guestfs_checksum_ret guestfs_checksum_ret;

struct guestfs_tar_in_args {
	char *directory;
};
typedef struct guestfs_tar_in_args guestfs_tar_in_args;

struct guestfs_tar_out_args {
	char *directory;
};
typedef struct guestfs_tar_out_args guestfs_tar_out_args;

struct guestfs_tgz_in_args {
	char *directory;
};
typedef struct guestfs_tgz_in_args guestfs_tgz_in_args;

struct guestfs_tgz_out_args {
	char *directory;
};
typedef struct guestfs_tgz_out_args guestfs_tgz_out_args;

struct guestfs_mount_ro_args {
	char *device;
	char *mountpoint;
};
typedef struct guestfs_mount_ro_args guestfs_mount_ro_args;

struct guestfs_mount_options_args {
	char *options;
	char *device;
	char *mountpoint;
};
typedef struct guestfs_mount_options_args guestfs_mount_options_args;

struct guestfs_mount_vfs_args {
	char *options;
	char *vfstype;
	char *device;
	char *mountpoint;
};
typedef struct guestfs_mount_vfs_args guestfs_mount_vfs_args;

struct guestfs_debug_args {
	char *subcmd;
	struct {
		u_int extraargs_len;
		str *extraargs_val;
	} extraargs;
};
typedef struct guestfs_debug_args guestfs_debug_args;

struct guestfs_debug_ret {
	char *result;
};
typedef struct guestfs_debug_ret guestfs_debug_ret;

enum guestfs_procedure {
	GUESTFS_PROC_MOUNT = 1,
	GUESTFS_PROC_SYNC = 2,
	GUESTFS_PROC_TOUCH = 3,
	GUESTFS_PROC_CAT = 4,
	GUESTFS_PROC_LL = 5,
	GUESTFS_PROC_LS = 6,
	GUESTFS_PROC_LIST_DEVICES = 7,
	GUESTFS_PROC_LIST_PARTITIONS = 8,
	GUESTFS_PROC_PVS = 9,
	GUESTFS_PROC_VGS = 10,
	GUESTFS_PROC_LVS = 11,
	GUESTFS_PROC_PVS_FULL = 12,
	GUESTFS_PROC_VGS_FULL = 13,
	GUESTFS_PROC_LVS_FULL = 14,
	GUESTFS_PROC_READ_LINES = 15,
	GUESTFS_PROC_AUG_INIT = 16,
	GUESTFS_PROC_AUG_CLOSE = 26,
	GUESTFS_PROC_AUG_DEFVAR = 17,
	GUESTFS_PROC_AUG_DEFNODE = 18,
	GUESTFS_PROC_AUG_GET = 19,
	GUESTFS_PROC_AUG_SET = 20,
	GUESTFS_PROC_AUG_INSERT = 21,
	GUESTFS_PROC_AUG_RM = 22,
	GUESTFS_PROC_AUG_MV = 23,
	GUESTFS_PROC_AUG_MATCH = 24,
	GUESTFS_PROC_AUG_SAVE = 25,
	GUESTFS_PROC_AUG_LOAD = 27,
	GUESTFS_PROC_AUG_LS = 28,
	GUESTFS_PROC_RM = 29,
	GUESTFS_PROC_RMDIR = 30,
	GUESTFS_PROC_RM_RF = 31,
	GUESTFS_PROC_MKDIR = 32,
	GUESTFS_PROC_MKDIR_P = 33,
	GUESTFS_PROC_CHMOD = 34,
	GUESTFS_PROC_CHOWN = 35,
	GUESTFS_PROC_EXISTS = 36,
	GUESTFS_PROC_IS_FILE = 37,
	GUESTFS_PROC_IS_DIR = 38,
	GUESTFS_PROC_PVCREATE = 39,
	GUESTFS_PROC_VGCREATE = 40,
	GUESTFS_PROC_LVCREATE = 41,
	GUESTFS_PROC_MKFS = 42,
	GUESTFS_PROC_SFDISK = 43,
	GUESTFS_PROC_WRITE_FILE = 44,
	GUESTFS_PROC_UMOUNT = 45,
	GUESTFS_PROC_MOUNTS = 46,
	GUESTFS_PROC_UMOUNT_ALL = 47,
	GUESTFS_PROC_LVM_REMOVE_ALL = 48,
	GUESTFS_PROC_FILE = 49,
	GUESTFS_PROC_COMMAND = 50,
	GUESTFS_PROC_COMMAND_LINES = 51,
	GUESTFS_PROC_STAT = 52,
	GUESTFS_PROC_LSTAT = 53,
	GUESTFS_PROC_STATVFS = 54,
	GUESTFS_PROC_TUNE2FS_L = 55,
	GUESTFS_PROC_BLOCKDEV_SETRO = 56,
	GUESTFS_PROC_BLOCKDEV_SETRW = 57,
	GUESTFS_PROC_BLOCKDEV_GETRO = 58,
	GUESTFS_PROC_BLOCKDEV_GETSS = 59,
	GUESTFS_PROC_BLOCKDEV_GETBSZ = 60,
	GUESTFS_PROC_BLOCKDEV_SETBSZ = 61,
	GUESTFS_PROC_BLOCKDEV_GETSZ = 62,
	GUESTFS_PROC_BLOCKDEV_GETSIZE64 = 63,
	GUESTFS_PROC_BLOCKDEV_FLUSHBUFS = 64,
	GUESTFS_PROC_BLOCKDEV_REREADPT = 65,
	GUESTFS_PROC_UPLOAD = 66,
	GUESTFS_PROC_DOWNLOAD = 67,
	GUESTFS_PROC_CHECKSUM = 68,
	GUESTFS_PROC_TAR_IN = 69,
	GUESTFS_PROC_TAR_OUT = 70,
	GUESTFS_PROC_TGZ_IN = 71,
	GUESTFS_PROC_TGZ_OUT = 72,
	GUESTFS_PROC_MOUNT_RO = 73,
	GUESTFS_PROC_MOUNT_OPTIONS = 74,
	GUESTFS_PROC_MOUNT_VFS = 75,
	GUESTFS_PROC_DEBUG = 76,
	GUESTFS_PROC_NR_PROCS = 76 + 1,
};
typedef enum guestfs_procedure guestfs_procedure;
#define GUESTFS_MESSAGE_MAX 4194304
#define GUESTFS_PROGRAM 0x2000F5F5
#define GUESTFS_PROTOCOL_VERSION 1
#define GUESTFS_LAUNCH_FLAG 0xf5f55ff5
#define GUESTFS_CANCEL_FLAG 0xffffeeee

enum guestfs_message_direction {
	GUESTFS_DIRECTION_CALL = 0,
	GUESTFS_DIRECTION_REPLY = 1,
};
typedef enum guestfs_message_direction guestfs_message_direction;

enum guestfs_message_status {
	GUESTFS_STATUS_OK = 0,
	GUESTFS_STATUS_ERROR = 1,
};
typedef enum guestfs_message_status guestfs_message_status;
#define GUESTFS_ERROR_LEN 256

struct guestfs_message_error {
	char *error_message;
};
typedef struct guestfs_message_error guestfs_message_error;

struct guestfs_message_header {
	u_int prog;
	u_int vers;
	guestfs_procedure proc;
	guestfs_message_direction direction;
	u_int serial;
	guestfs_message_status status;
};
typedef struct guestfs_message_header guestfs_message_header;
#define GUESTFS_MAX_CHUNK_SIZE 8192

struct guestfs_chunk {
	int cancel;
	struct {
		u_int data_len;
		char *data_val;
	} data;
};
typedef struct guestfs_chunk guestfs_chunk;

/* the xdr functions */

#if defined(__STDC__) || defined(__cplusplus)
extern  bool_t xdr_str (XDR *, str*);
extern  bool_t xdr_guestfs_lvm_int_pv (XDR *, guestfs_lvm_int_pv*);
extern  bool_t xdr_guestfs_lvm_int_pv_list (XDR *, guestfs_lvm_int_pv_list*);
extern  bool_t xdr_guestfs_lvm_int_vg (XDR *, guestfs_lvm_int_vg*);
extern  bool_t xdr_guestfs_lvm_int_vg_list (XDR *, guestfs_lvm_int_vg_list*);
extern  bool_t xdr_guestfs_lvm_int_lv (XDR *, guestfs_lvm_int_lv*);
extern  bool_t xdr_guestfs_lvm_int_lv_list (XDR *, guestfs_lvm_int_lv_list*);
extern  bool_t xdr_guestfs_int_stat (XDR *, guestfs_int_stat*);
extern  bool_t xdr_guestfs_int_statvfs (XDR *, guestfs_int_statvfs*);
extern  bool_t xdr_guestfs_mount_args (XDR *, guestfs_mount_args*);
extern  bool_t xdr_guestfs_touch_args (XDR *, guestfs_touch_args*);
extern  bool_t xdr_guestfs_cat_args (XDR *, guestfs_cat_args*);
extern  bool_t xdr_guestfs_cat_ret (XDR *, guestfs_cat_ret*);
extern  bool_t xdr_guestfs_ll_args (XDR *, guestfs_ll_args*);
extern  bool_t xdr_guestfs_ll_ret (XDR *, guestfs_ll_ret*);
extern  bool_t xdr_guestfs_ls_args (XDR *, guestfs_ls_args*);
extern  bool_t xdr_guestfs_ls_ret (XDR *, guestfs_ls_ret*);
extern  bool_t xdr_guestfs_list_devices_ret (XDR *, guestfs_list_devices_ret*);
extern  bool_t xdr_guestfs_list_partitions_ret (XDR *, guestfs_list_partitions_ret*);
extern  bool_t xdr_guestfs_pvs_ret (XDR *, guestfs_pvs_ret*);
extern  bool_t xdr_guestfs_vgs_ret (XDR *, guestfs_vgs_ret*);
extern  bool_t xdr_guestfs_lvs_ret (XDR *, guestfs_lvs_ret*);
extern  bool_t xdr_guestfs_pvs_full_ret (XDR *, guestfs_pvs_full_ret*);
extern  bool_t xdr_guestfs_vgs_full_ret (XDR *, guestfs_vgs_full_ret*);
extern  bool_t xdr_guestfs_lvs_full_ret (XDR *, guestfs_lvs_full_ret*);
extern  bool_t xdr_guestfs_read_lines_args (XDR *, guestfs_read_lines_args*);
extern  bool_t xdr_guestfs_read_lines_ret (XDR *, guestfs_read_lines_ret*);
extern  bool_t xdr_guestfs_aug_init_args (XDR *, guestfs_aug_init_args*);
extern  bool_t xdr_guestfs_aug_defvar_args (XDR *, guestfs_aug_defvar_args*);
extern  bool_t xdr_guestfs_aug_defvar_ret (XDR *, guestfs_aug_defvar_ret*);
extern  bool_t xdr_guestfs_aug_defnode_args (XDR *, guestfs_aug_defnode_args*);
extern  bool_t xdr_guestfs_aug_defnode_ret (XDR *, guestfs_aug_defnode_ret*);
extern  bool_t xdr_guestfs_aug_get_args (XDR *, guestfs_aug_get_args*);
extern  bool_t xdr_guestfs_aug_get_ret (XDR *, guestfs_aug_get_ret*);
extern  bool_t xdr_guestfs_aug_set_args (XDR *, guestfs_aug_set_args*);
extern  bool_t xdr_guestfs_aug_insert_args (XDR *, guestfs_aug_insert_args*);
extern  bool_t xdr_guestfs_aug_rm_args (XDR *, guestfs_aug_rm_args*);
extern  bool_t xdr_guestfs_aug_rm_ret (XDR *, guestfs_aug_rm_ret*);
extern  bool_t xdr_guestfs_aug_mv_args (XDR *, guestfs_aug_mv_args*);
extern  bool_t xdr_guestfs_aug_match_args (XDR *, guestfs_aug_match_args*);
extern  bool_t xdr_guestfs_aug_match_ret (XDR *, guestfs_aug_match_ret*);
extern  bool_t xdr_guestfs_aug_ls_args (XDR *, guestfs_aug_ls_args*);
extern  bool_t xdr_guestfs_aug_ls_ret (XDR *, guestfs_aug_ls_ret*);
extern  bool_t xdr_guestfs_rm_args (XDR *, guestfs_rm_args*);
extern  bool_t xdr_guestfs_rmdir_args (XDR *, guestfs_rmdir_args*);
extern  bool_t xdr_guestfs_rm_rf_args (XDR *, guestfs_rm_rf_args*);
extern  bool_t xdr_guestfs_mkdir_args (XDR *, guestfs_mkdir_args*);
extern  bool_t xdr_guestfs_mkdir_p_args (XDR *, guestfs_mkdir_p_args*);
extern  bool_t xdr_guestfs_chmod_args (XDR *, guestfs_chmod_args*);
extern  bool_t xdr_guestfs_chown_args (XDR *, guestfs_chown_args*);
extern  bool_t xdr_guestfs_exists_args (XDR *, guestfs_exists_args*);
extern  bool_t xdr_guestfs_exists_ret (XDR *, guestfs_exists_ret*);
extern  bool_t xdr_guestfs_is_file_args (XDR *, guestfs_is_file_args*);
extern  bool_t xdr_guestfs_is_file_ret (XDR *, guestfs_is_file_ret*);
extern  bool_t xdr_guestfs_is_dir_args (XDR *, guestfs_is_dir_args*);
extern  bool_t xdr_guestfs_is_dir_ret (XDR *, guestfs_is_dir_ret*);
extern  bool_t xdr_guestfs_pvcreate_args (XDR *, guestfs_pvcreate_args*);
extern  bool_t xdr_guestfs_vgcreate_args (XDR *, guestfs_vgcreate_args*);
extern  bool_t xdr_guestfs_lvcreate_args (XDR *, guestfs_lvcreate_args*);
extern  bool_t xdr_guestfs_mkfs_args (XDR *, guestfs_mkfs_args*);
extern  bool_t xdr_guestfs_sfdisk_args (XDR *, guestfs_sfdisk_args*);
extern  bool_t xdr_guestfs_write_file_args (XDR *, guestfs_write_file_args*);
extern  bool_t xdr_guestfs_umount_args (XDR *, guestfs_umount_args*);
extern  bool_t xdr_guestfs_mounts_ret (XDR *, guestfs_mounts_ret*);
extern  bool_t xdr_guestfs_file_args (XDR *, guestfs_file_args*);
extern  bool_t xdr_guestfs_file_ret (XDR *, guestfs_file_ret*);
extern  bool_t xdr_guestfs_command_args (XDR *, guestfs_command_args*);
extern  bool_t xdr_guestfs_command_ret (XDR *, guestfs_command_ret*);
extern  bool_t xdr_guestfs_command_lines_args (XDR *, guestfs_command_lines_args*);
extern  bool_t xdr_guestfs_command_lines_ret (XDR *, guestfs_command_lines_ret*);
extern  bool_t xdr_guestfs_stat_args (XDR *, guestfs_stat_args*);
extern  bool_t xdr_guestfs_stat_ret (XDR *, guestfs_stat_ret*);
extern  bool_t xdr_guestfs_lstat_args (XDR *, guestfs_lstat_args*);
extern  bool_t xdr_guestfs_lstat_ret (XDR *, guestfs_lstat_ret*);
extern  bool_t xdr_guestfs_statvfs_args (XDR *, guestfs_statvfs_args*);
extern  bool_t xdr_guestfs_statvfs_ret (XDR *, guestfs_statvfs_ret*);
extern  bool_t xdr_guestfs_tune2fs_l_args (XDR *, guestfs_tune2fs_l_args*);
extern  bool_t xdr_guestfs_tune2fs_l_ret (XDR *, guestfs_tune2fs_l_ret*);
extern  bool_t xdr_guestfs_blockdev_setro_args (XDR *, guestfs_blockdev_setro_args*);
extern  bool_t xdr_guestfs_blockdev_setrw_args (XDR *, guestfs_blockdev_setrw_args*);
extern  bool_t xdr_guestfs_blockdev_getro_args (XDR *, guestfs_blockdev_getro_args*);
extern  bool_t xdr_guestfs_blockdev_getro_ret (XDR *, guestfs_blockdev_getro_ret*);
extern  bool_t xdr_guestfs_blockdev_getss_args (XDR *, guestfs_blockdev_getss_args*);
extern  bool_t xdr_guestfs_blockdev_getss_ret (XDR *, guestfs_blockdev_getss_ret*);
extern  bool_t xdr_guestfs_blockdev_getbsz_args (XDR *, guestfs_blockdev_getbsz_args*);
extern  bool_t xdr_guestfs_blockdev_getbsz_ret (XDR *, guestfs_blockdev_getbsz_ret*);
extern  bool_t xdr_guestfs_blockdev_setbsz_args (XDR *, guestfs_blockdev_setbsz_args*);
extern  bool_t xdr_guestfs_blockdev_getsz_args (XDR *, guestfs_blockdev_getsz_args*);
extern  bool_t xdr_guestfs_blockdev_getsz_ret (XDR *, guestfs_blockdev_getsz_ret*);
extern  bool_t xdr_guestfs_blockdev_getsize64_args (XDR *, guestfs_blockdev_getsize64_args*);
extern  bool_t xdr_guestfs_blockdev_getsize64_ret (XDR *, guestfs_blockdev_getsize64_ret*);
extern  bool_t xdr_guestfs_blockdev_flushbufs_args (XDR *, guestfs_blockdev_flushbufs_args*);
extern  bool_t xdr_guestfs_blockdev_rereadpt_args (XDR *, guestfs_blockdev_rereadpt_args*);
extern  bool_t xdr_guestfs_upload_args (XDR *, guestfs_upload_args*);
extern  bool_t xdr_guestfs_download_args (XDR *, guestfs_download_args*);
extern  bool_t xdr_guestfs_checksum_args (XDR *, guestfs_checksum_args*);
extern  bool_t xdr_guestfs_checksum_ret (XDR *, guestfs_checksum_ret*);
extern  bool_t xdr_guestfs_tar_in_args (XDR *, guestfs_tar_in_args*);
extern  bool_t xdr_guestfs_tar_out_args (XDR *, guestfs_tar_out_args*);
extern  bool_t xdr_guestfs_tgz_in_args (XDR *, guestfs_tgz_in_args*);
extern  bool_t xdr_guestfs_tgz_out_args (XDR *, guestfs_tgz_out_args*);
extern  bool_t xdr_guestfs_mount_ro_args (XDR *, guestfs_mount_ro_args*);
extern  bool_t xdr_guestfs_mount_options_args (XDR *, guestfs_mount_options_args*);
extern  bool_t xdr_guestfs_mount_vfs_args (XDR *, guestfs_mount_vfs_args*);
extern  bool_t xdr_guestfs_debug_args (XDR *, guestfs_debug_args*);
extern  bool_t xdr_guestfs_debug_ret (XDR *, guestfs_debug_ret*);
extern  bool_t xdr_guestfs_procedure (XDR *, guestfs_procedure*);
extern  bool_t xdr_guestfs_message_direction (XDR *, guestfs_message_direction*);
extern  bool_t xdr_guestfs_message_status (XDR *, guestfs_message_status*);
extern  bool_t xdr_guestfs_message_error (XDR *, guestfs_message_error*);
extern  bool_t xdr_guestfs_message_header (XDR *, guestfs_message_header*);
extern  bool_t xdr_guestfs_chunk (XDR *, guestfs_chunk*);

#else /* K&R C */
extern bool_t xdr_str ();
extern bool_t xdr_guestfs_lvm_int_pv ();
extern bool_t xdr_guestfs_lvm_int_pv_list ();
extern bool_t xdr_guestfs_lvm_int_vg ();
extern bool_t xdr_guestfs_lvm_int_vg_list ();
extern bool_t xdr_guestfs_lvm_int_lv ();
extern bool_t xdr_guestfs_lvm_int_lv_list ();
extern bool_t xdr_guestfs_int_stat ();
extern bool_t xdr_guestfs_int_statvfs ();
extern bool_t xdr_guestfs_mount_args ();
extern bool_t xdr_guestfs_touch_args ();
extern bool_t xdr_guestfs_cat_args ();
extern bool_t xdr_guestfs_cat_ret ();
extern bool_t xdr_guestfs_ll_args ();
extern bool_t xdr_guestfs_ll_ret ();
extern bool_t xdr_guestfs_ls_args ();
extern bool_t xdr_guestfs_ls_ret ();
extern bool_t xdr_guestfs_list_devices_ret ();
extern bool_t xdr_guestfs_list_partitions_ret ();
extern bool_t xdr_guestfs_pvs_ret ();
extern bool_t xdr_guestfs_vgs_ret ();
extern bool_t xdr_guestfs_lvs_ret ();
extern bool_t xdr_guestfs_pvs_full_ret ();
extern bool_t xdr_guestfs_vgs_full_ret ();
extern bool_t xdr_guestfs_lvs_full_ret ();
extern bool_t xdr_guestfs_read_lines_args ();
extern bool_t xdr_guestfs_read_lines_ret ();
extern bool_t xdr_guestfs_aug_init_args ();
extern bool_t xdr_guestfs_aug_defvar_args ();
extern bool_t xdr_guestfs_aug_defvar_ret ();
extern bool_t xdr_guestfs_aug_defnode_args ();
extern bool_t xdr_guestfs_aug_defnode_ret ();
extern bool_t xdr_guestfs_aug_get_args ();
extern bool_t xdr_guestfs_aug_get_ret ();
extern bool_t xdr_guestfs_aug_set_args ();
extern bool_t xdr_guestfs_aug_insert_args ();
extern bool_t xdr_guestfs_aug_rm_args ();
extern bool_t xdr_guestfs_aug_rm_ret ();
extern bool_t xdr_guestfs_aug_mv_args ();
extern bool_t xdr_guestfs_aug_match_args ();
extern bool_t xdr_guestfs_aug_match_ret ();
extern bool_t xdr_guestfs_aug_ls_args ();
extern bool_t xdr_guestfs_aug_ls_ret ();
extern bool_t xdr_guestfs_rm_args ();
extern bool_t xdr_guestfs_rmdir_args ();
extern bool_t xdr_guestfs_rm_rf_args ();
extern bool_t xdr_guestfs_mkdir_args ();
extern bool_t xdr_guestfs_mkdir_p_args ();
extern bool_t xdr_guestfs_chmod_args ();
extern bool_t xdr_guestfs_chown_args ();
extern bool_t xdr_guestfs_exists_args ();
extern bool_t xdr_guestfs_exists_ret ();
extern bool_t xdr_guestfs_is_file_args ();
extern bool_t xdr_guestfs_is_file_ret ();
extern bool_t xdr_guestfs_is_dir_args ();
extern bool_t xdr_guestfs_is_dir_ret ();
extern bool_t xdr_guestfs_pvcreate_args ();
extern bool_t xdr_guestfs_vgcreate_args ();
extern bool_t xdr_guestfs_lvcreate_args ();
extern bool_t xdr_guestfs_mkfs_args ();
extern bool_t xdr_guestfs_sfdisk_args ();
extern bool_t xdr_guestfs_write_file_args ();
extern bool_t xdr_guestfs_umount_args ();
extern bool_t xdr_guestfs_mounts_ret ();
extern bool_t xdr_guestfs_file_args ();
extern bool_t xdr_guestfs_file_ret ();
extern bool_t xdr_guestfs_command_args ();
extern bool_t xdr_guestfs_command_ret ();
extern bool_t xdr_guestfs_command_lines_args ();
extern bool_t xdr_guestfs_command_lines_ret ();
extern bool_t xdr_guestfs_stat_args ();
extern bool_t xdr_guestfs_stat_ret ();
extern bool_t xdr_guestfs_lstat_args ();
extern bool_t xdr_guestfs_lstat_ret ();
extern bool_t xdr_guestfs_statvfs_args ();
extern bool_t xdr_guestfs_statvfs_ret ();
extern bool_t xdr_guestfs_tune2fs_l_args ();
extern bool_t xdr_guestfs_tune2fs_l_ret ();
extern bool_t xdr_guestfs_blockdev_setro_args ();
extern bool_t xdr_guestfs_blockdev_setrw_args ();
extern bool_t xdr_guestfs_blockdev_getro_args ();
extern bool_t xdr_guestfs_blockdev_getro_ret ();
extern bool_t xdr_guestfs_blockdev_getss_args ();
extern bool_t xdr_guestfs_blockdev_getss_ret ();
extern bool_t xdr_guestfs_blockdev_getbsz_args ();
extern bool_t xdr_guestfs_blockdev_getbsz_ret ();
extern bool_t xdr_guestfs_blockdev_setbsz_args ();
extern bool_t xdr_guestfs_blockdev_getsz_args ();
extern bool_t xdr_guestfs_blockdev_getsz_ret ();
extern bool_t xdr_guestfs_blockdev_getsize64_args ();
extern bool_t xdr_guestfs_blockdev_getsize64_ret ();
extern bool_t xdr_guestfs_blockdev_flushbufs_args ();
extern bool_t xdr_guestfs_blockdev_rereadpt_args ();
extern bool_t xdr_guestfs_upload_args ();
extern bool_t xdr_guestfs_download_args ();
extern bool_t xdr_guestfs_checksum_args ();
extern bool_t xdr_guestfs_checksum_ret ();
extern bool_t xdr_guestfs_tar_in_args ();
extern bool_t xdr_guestfs_tar_out_args ();
extern bool_t xdr_guestfs_tgz_in_args ();
extern bool_t xdr_guestfs_tgz_out_args ();
extern bool_t xdr_guestfs_mount_ro_args ();
extern bool_t xdr_guestfs_mount_options_args ();
extern bool_t xdr_guestfs_mount_vfs_args ();
extern bool_t xdr_guestfs_debug_args ();
extern bool_t xdr_guestfs_debug_ret ();
extern bool_t xdr_guestfs_procedure ();
extern bool_t xdr_guestfs_message_direction ();
extern bool_t xdr_guestfs_message_status ();
extern bool_t xdr_guestfs_message_error ();
extern bool_t xdr_guestfs_message_header ();
extern bool_t xdr_guestfs_chunk ();

#endif /* K&R C */

#ifdef __cplusplus
}
#endif

#endif /* !_GUESTFS_PROTOCOL_H_RPCGEN */
