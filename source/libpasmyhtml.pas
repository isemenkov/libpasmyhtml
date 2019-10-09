(******************************************************************************)
(*                                libPasMyHTML                                *)
(*                object pascal wrapper around MyHTML library                 *)
(*                    https://github.com/lexborisov/myhtml                    *)
(*                                                                            *)
(* Copyright (c) 2019                                       Ivan Semenkov     *)
(* https://github.com/isemenkov/libpasmyhtml                ivan@semenkov.pro *)
(*                                                          Ukraine           *)
(******************************************************************************)
(*                                                                            *)
(* This source  is free software;  you can redistribute  it and/or modify  it *)
(* under the terms of the GNU General Public License as published by the Free *)
(* Software Foundation; either version 3 of the License.                      *)
(*                                                                            *)
(* This code is distributed in the  hope that it will  be useful, but WITHOUT *)
(* ANY  WARRANTY;  without even  the implied  warranty of MERCHANTABILITY  or *)
(* FITNESS FOR A PARTICULAR PURPOSE.  See the  GNU General Public License for *)
(* more details.                                                              *)
(*                                                                            *)
(* A copy  of the  GNU General Public License is available  on the World Wide *)
(* Web at <http://www.gnu.org/copyleft/gpl.html>. You  can also obtain  it by *)
(* writing to the Free Software Foundation, Inc., 51  Franklin Street - Fifth *)
(* Floor, Boston, MA 02110-1335, USA.                                         *)
(*                                                                            *)
(******************************************************************************)

unit libpasmyhtml;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

{$IFDEF FPC}
  {$PACKRECORDS C}
{$ENDIF}

{$IFDEF WINDOWS}
  const MyHTMLLib = 'libmyhtml.dll';
{$ENDIF}
{$IFDEF LINUX}
  const MyHTMLLib = 'libmyhtml.so';
{$ENDIF}

type
  PPChar = ^PChar;

(*mycore/myosi.h***************************************************************)

type
  pmystatus_t = ^mystatus_t;
  mystatus_t = type Cardinal;

  pmythread_id_t = ^mythread_id_t;
  mythread_id_t = type QWord;

(*mycore/utils.h***************************************************************)

function mycore_power (t : QWord; k : QWord) : QWord; cdecl; external MyHTMLLib;
function mycore_strncasecmp (const str1 : PChar; const str2 : PChar; size :
  QWord) : QWord; cdecl; external MyHTMLLib;
function mycore_strcasecmp (const str1 : PChar; const str2 : PChar) : QWord;
  cdecl; external MyHTMLLib;
function mycore_strncmp (const str1 : PChar; const str2 : PChar; size :
  QWord) : QWord; cdecl; external MyHTMLLib;
function mycore_strcmp (const str1 : PChar; const str2 : PChar) : QWord; cdecl;
  external MyHTMLLib;
function mycore_strcmp_ws (const str1 : PChar; const str2 : PChar) : QWord;
  cdecl; external MyHTMLLib;
function mycore_ustrcasecmp_without_checks_by_secondary (const ustr1 : PByte;
  const ustr2 : PByte) : Boolean; cdecl; external MyHTMLLib;

(*mycore/utils/mcobject.h******************************************************)

type
  pmcobject_chunk_t = ^mcobject_chunk_t;
  mcobject_chunk_t = record
    start : PByte;
    length : QWord;
    size : QWord;

    next : pmcobject_chunk_t;
    prev : pmcobject_chunk_t;
  end;

  pmcobject_t = ^mcobject_t;
  mcobject_t = record
    chunk : pmcobject_chunk_t;
    cache : Pointer;
    cache_size : QWord;
    cache_length : QWord;

    struct_size : QWord;
    chunk_size : QWord;
  end;

function mcobject_create : pmcobject_t; cdecl; external MyHTMLLib;
function mcobject_init (mcobject : pmcobject_t; chunk_size : QWord;
  struct_size : QWord ) : mystatus_t; cdecl; external MyHTMLLib;
procedure mcobject_clean (mcobject : pmcobject_t); cdecl; external MyHTMLLib;
function mcobject_destroy (mcobject : pmcobject_t; destroy_self : Boolean) :
  pmcobject_t; cdecl; external MyHTMLLib;
procedure mcobject_chunk_malloc (mcobject : pmcobject_t; status : pmystatus_t);
  cdecl; external MyHTMLLib;
function mcobject_malloc (mcobject : pmcobject_t; status : pmystatus_t) :
  Pointer; cdecl; external MyHTMLLib;
function mcobject_free (mcobject : pmcobject_t; entry : Pointer) : mystatus_t;
  cdecl; external MyHTMLLib;

(*mycore/utils/avl_tree.h******************************************************)

type
  ppmycore_utils_avl_tree_node_t = ^pmycore_utils_avl_tree_node_t;
  pmycore_utils_avl_tree_node_t = ^mycore_utils_avl_tree_node_t;
  mycore_utils_avl_tree_node_t = record
    value : Pointer;
    node_type : QWord;

    left : pmycore_utils_avl_tree_node_t;
    rigth : pmycore_utils_avl_tree_node_t;
    parent : pmycore_utils_avl_tree_node_t;

    height : SmallInt;
  end;

  pmycore_utils_avl_tree_t = ^mycore_utils_avl_tree_t;
  mycore_utils_avl_tree_t = record
    mc_nodes : pmcobject_t;
  end;

  mycore_utils_avl_tree_node_callback_f = procedure (avl_tree_node :
    pmycore_utils_avl_tree_node_t; ctx : Pointer) of object;

function mycore_utils_avl_tree_create : pmycore_utils_avl_tree_t; cdecl;
  external MyHTMLLib;
function mycore_utils_avl_tree_init (avl_tree : pmycore_utils_avl_tree_t)
  : mystatus_t; cdecl; external MyHTMLLib;
procedure mycore_utils_avl_tree_clean (avl_tree : pmycore_utils_avl_tree_t);
  cdecl; external MyHTMLLib;
function mycore_utils_avl_tree_destroy (avl_tree : pmycore_utils_avl_tree_t;
  self_destroy : Boolean) : pmycore_utils_avl_tree_t; cdecl; external MyHTMLLib;
function mycore_utils_avl_tree_node_create_root (avl_tree :
  pmycore_utils_avl_tree_t; node_type : QWord; value : Pointer)
  : pmycore_utils_avl_tree_node_t; cdecl; external MyHTMLLib;
procedure mycore_utils_avl_tree_add (avl_tree : pmycore_utils_avl_tree_t;
  root : ppmycore_utils_avl_tree_node_t; node_type : QWord; value : Pointer);
  cdecl; external MyHTMLLib;
function mycore_utils_avl_tree_delete (avl_tree : pmycore_utils_avl_tree_t;
  root : ppmycore_utils_avl_tree_node_t; node_type : QWord) : Pointer; cdecl;
  external MyHTMLLib;
function mycore_utils_avl_tree_search_by_type (avl_tree :
  pmycore_utils_avl_tree_t; node : pmycore_utils_avl_tree_node_t; node_type :
  QWord) : pmycore_utils_avl_tree_node_t; cdecl; external MyHTMLLib;
procedure mycore_utils_avl_tree_list_all_nodes (avl_tree :
  pmycore_utils_avl_tree_t; root : pmycore_utils_avl_tree_node_t; callback :
  mycore_utils_avl_tree_node_callback_f; ctx : Pointer); cdecl;
  external MyHTMLLib;

(*mycore/utils/mcsimple.h******************************************************)

type
  pmcsimple_t = ^mcsimple_t;
  mcsimple_t = record
    struct_size : QWord;
    list : PByte;
    list_pos_size : QWord;
    list_pos_length : QWord;
    list_pos_length_used : QWord;
    list_size : QWord;
    list_length : QWord;
  end;

function mcsimple_create : pmcsimple_t; cdecl; external MyHTMLLib;
procedure mcsimple_init (mcsimple : pmcsimple_t; pos_size : QWord;
  list_size : QWord; struct_size : QWord); cdecl; external MyHTMLLib;
procedure mcsimple_clean (mcsimple : pmcsimple_t); cdecl; external MyHTMLLib;
function mcsimple_destroy (mcsimple : pmcsimple_t; destroy_self : Boolean)
  : pmcsimple_t; cdecl; external MyHTMLLib;
function mcsimple_init_list_entries (mcsimple : pmcsimple_t; pos : QWord)
  : PByte; cdecl; external MyHTMLLib;
function mcsimple_malloc (mcsimple : pmcsimple_t) : Pointer; cdecl;
  external MyHTMLLib;
function mcsimple_get_by_absolute_position (mcsimple : pmcsimple_t;
  pos : QWord) : Pointer; cdecl; external MyHTMLLib;

(*mycore/utils/mcsync.h********************************************************)

type
  pmcsync_status_t = ^mcsync_status_t;
  mcsync_status_t = (
    MCSYNC_STATUS_OK                                                    = $0000,
    MCSYNC_STATUS_NOT_OK                                                = $0001,
    MCSYNC_STATUS_ERROR_MEM_ALLOCATE                                    = $0002
  );

  pmcsync_t = ^mcsync_t;
  mcsync_t = record
    spinlock : PInteger;
    mutex : Pointer;
  end;

function mcsync_create : pmcsync_t; cdecl; external MyHTMLLib;
function mcsync_init (mcsync : pmcsync_t) : mcsync_status_t; cdecl;
  external MyHTMLLib;
procedure mcsync_clean (mcsync : pmcsync_t); cdecl; external MyHTMLLib;
function mcsync_destroy (mcsync : pmcsync_t; destroy_self : Integer)
  : pmcsync_t; cdecl; external MyHTMLLib;
function mcsync_lock (mcsync : pmcsync_t) : mcsync_status_t; cdecl;
  external MyHTMLLib;
function mcsync_unlock (mcsync : pmcsync_t) : mcsync_status_t; cdecl;
  external MyHTMLLib;

{$IFNDEF MyCORE_BUILD_WITHOUT_THREADS}
function mcsync_spin_lock (spinlock : Pointer) : mcsync_status_t; cdecl;
  external MyHTMLLib;
function mcsync_spin_unlock (spinlock : Pointer) : mcsync_status_t; cdecl;
  external MyHTMLLib;
function mcsync_mutex_lock (mutex : Pointer) : mcsync_status_t; cdecl;
  external MyHTMLLib;
function mcsync_mutex_try_lock (mutex : Pointer) : mcsync_status_t; cdecl;
  external MyHTMLLib;
function mcsync_mutex_unlock (mutex : Pointer) : mcsync_status_t; cdecl;
  external MyHTMLLib;
function mcsync_spin_create : Pointer; cdecl; external MyHTMLLib;
function mcsync_spin_init (spiclock : Pointer) : mcsync_status_t; cdecl;
  external MyHTMLLib;
procedure mcsync_spin_clear (spinlock : Pointer); cdecl; external MyHTMLLib;
procedure mcsync_spin_destroy (spinlock : Pointer); cdecl; external MyHTMLLib;
function mcsync_mutex_create : Pointer; cdecl; external MyHTMLLib;
function mcsync_mutex_init (mutex : Pointer) : mcsync_status_t; cdecl;
  external MyHTMLLib;
procedure mcsync_mutex_clear (mutex : Pointer); cdecl; external MyHTMLLib;
procedure mcsync_mutex_destroy (mutex : Pointer); cdecl; external MyHTMLLib;
{$ENDIF}{MyCORE_BUILD_WITHOUT_THREADS}

(*mycore/utils/mcobject_async.h************************************************)

type
  pmcobject_async_status_t = ^mcobject_async_status_t;
  mcobject_async_status_t = (
    MCOBJECT_ASYNC_STATUS_OK                                            = 0,
    MCOBJECT_ASYNC_STATUS_ERROR_MEMORY_ALLOCATION                       = 1,
    MCOBJECT_ASYNC_STATUS_CHUNK_ERROR_MEMORY_ALLOCATION                 = 2,
    MCOBJECT_ASYNC_STATUS_CHUNK_CACHE_ERROR_MEMORY_ALLOCATION           = 3,
    MCOBJECT_ASYNC_STATUS_NODES_ERROR_MEMORY_ALLOCATION                 = 4,
    MCOBJECT_ASYNC_STATUS_NODES_ERROR_BAD_NODE_ID                       = 5,
    MCOBJECT_ASYNC_STATUS_CACHE_ERROR_MEMORY_REALLOC                    = 6
  );

  ppmcobject_async_chunk_t = ^pmcobject_async_chunk_t;
  pmcobject_async_chunk_t = ^mcobject_async_chunk_t;
  mcobject_async_chunk_t = record
    start : PByte;
    length : QWord;
    size : QWord;

    next : pmcobject_async_chunk_t;
    prev : pmcobject_async_chunk_t;
  end;

  pmcobject_async_node_t = ^mcobject_async_node_t;
  mcobject_async_node_t = record
    chunk : pmcobject_async_chunk_t;
    cache : Pointer;
    cache_size : QWord;
    cache_length : QWord;
  end;

  pmcobject_async_t = ^mcobject_async_t;
  mcobject_async_t = record
    origin_size : QWord;
    struct_size : QWord;
    struct_size_sn : QWord;

    chunk_cache : ppmcobject_async_chunk_t;
    chunk_cache_size : QWord;
    chunk_cache_length : QWord;

    chunks : ppmcobject_async_chunk_t;
    chunks_pos_size : QWord;
    chunks_pos_length : QWord;
    chunks_size : QWord;
    chunks_length : QWord;

    nodes : pmcobject_async_node_t;
    nodes_length : QWord;
    nodes_size : QWord;

    nodes_cache : PQWord;
    nodes_cache_length : QWord;
    nodes_cache_size : QWord;
  end;

function mcobject_async_create : pmcobject_async_t; cdecl; external MyHTMLLib;
function mcobject_async_init (mcobj_async : pmcobject_async_t; chunk_len :
  QWord; obj_size_by_one_chunk : QWord; struct_size : QWord) :
  pmcobject_async_status_t; cdecl; external MyHTMLLib;
procedure mcobject_async_clean (mcobj_async : pmcobject_async_t); cdecl;
  external MyHTMLLib;
function mcobject_async_destroy (mcobj_async : pmcobject_async_t; destroy_self :
  Integer) : pmcobject_async_t; cdecl; external MyHTMLLib;
function mcobject_async_node_add (mcobj_async : pmcobject_async_t; status :
  pmcobject_async_status_t) : QWord; cdecl; external MyHTMLLib;
procedure mcobject_async_node_clean (mcobj_async : pmcobject_async_t; node_idx :
  QWord); cdecl; external MyHTMLLib;
procedure mcobject_async_node_all_clean (mcobj_async : pmcobject_async_t);
  cdecl; external MyHTMLLib;
procedure mcobject_async_node_delete (mcobj_async : pmcobject_async_t;
  node_idx : QWord); cdecl; external MyHTMLLib;
function mcobject_async_malloc (mcobj_async : pmcobject_async_t; node_idx :
  QWord; status : pmcobject_async_status_t) : Pointer; cdecl;
  external MyHTMLLib;
function mcobject_async_free (mcobj_async : pmcobject_async_t; entry : Pointer)
  : mcobject_async_status_t; cdecl; external MyHTMLLib;
function mcobject_async_chunk_malloc (mcobj_async : pmcobject_async_t; length :
  QWord; status : pmcobject_async_status_t) : pmcobject_async_chunk_t; cdecl;
  external MyHTMLLib;
function mcobject_async_chunk_malloc_without_lock (mcobj_async :
  pmcobject_async_t; length : QWord; status : pmcobject_async_status_t) :
  pmcobject_async_chunk_t; cdecl; external MyHTMLLib;

(*mycore/utils/mchar_async.h***************************************************)

type
  pmchar_async_cache_node_t = ^mchar_async_cache_node_t;
  mchar_async_cache_node_t = record
    value : Pointer;
    size : QWord;

    left : QWord;
    right : QWord;
    parent : QWord;
  end;

  ppmchar_async_chunk_t = ^pmchar_async_chunk_t;
  pmchar_async_chunk_t = ^mchar_async_chunk_t;
  mchar_async_chunk_t = record
    start : PChar;
    length : QWord;
    size : QWord;

    next : pmchar_async_chunk_t;
    prev : pmchar_async_chunk_t;
  end;

  pmchar_async_cache_t = ^mchar_async_cache_t;
  mchar_async_cache_t = record
    nodes : pmchar_async_cache_node_t;
    nodes_size : QWord;
    nodes_length : QWord;
    nodes_root : QWord;
    count : QWord;
    index : PQWord;
    index_length : QWord;
    index_size : QWord;
  end;

  pmchar_async_node_t = ^mchar_async_node_t;
  mchar_async_node_t = record
    chunk : pmchar_async_chunk_t;
    cache : mchar_async_cache_t;
  end;

  pmchar_async_t = ^mchar_async_t;
  mchar_async_t = record
    origin_size : QWord;

    chunks : ppmchar_async_chunk_t;
    chunks_pos_size : QWord;
    chunks_pos_length : QWord;
    chunks_size : QWord;
    chunks_length : QWord;

    chunk_cache : mchar_async_cache_t;

    nodes : pmchar_async_node_t;
    nodes_length : QWord;
    nodes_size : QWord;

    nodes_cache : PQWord;
    nodes_cache_length : QWord;
    nodes_cache_size : QWord;

    mcsync : pmcsync_t;
  end;

function mchar_async_create : pmchar_async_t; cdecl; external MyHTMLLib;
function mchar_async_init (mchar_async : pmchar_async_t; chunk_len : QWord;
  char_size : QWord) : mystatus_t; cdecl; external MyHTMLLib;
function mchar_async_clean (mchar_async : pmchar_async_t) : mystatus_t; cdecl;
  external MyHTMLLib;
function mchar_async_destroy (mchar_async : pmchar_async_t; destroy_self :
  Integer) : pmchar_async_t; cdecl; external MyHTMLLib;
function mchar_async_malloc (mchar_async : pmchar_async_t; node_idx : QWord;
  size : QWord) : PChar; cdecl; external MyHTMLLib;
function mchar_async_realloc (mchar_async : pmchar_async_t; node_idx : QWord;
  data : PChar; data_len : QWord; new_size : QWord) : PChar; cdecl;
  external MyHTMLLib;
procedure mchar_async_free (mchar_async : pmchar_async_t; node_idx : QWord;
  entry : PChar); cdecl; external MyHTMLLib;
function mchar_async_node_add (mchar_async : pmchar_async_t; status :
  pmystatus_t) : QWord; cdecl; external MyHTMLLib;
procedure mchar_async_node_clean (mchar_async : pmchar_async_t; node_idx :
  QWord); cdecl; external MyHTMLLib;
procedure mchar_async_node_delete (mchar_async : pmchar_async_t; node_idx :
  QWord); cdecl; external MyHTMLLib;
function mchar_async_chunk_malloc (mchar_async : pmchar_async_t; node :
  pmchar_async_node_t; length : QWord) : pmchar_async_chunk_t; cdecl;
  external MyHTMLLib;
function mchar_async_crop_first_chars (mchar_async : pmchar_async_t; node_idx :
  QWord; data : PChar; crop_len : QWord) : PChar; cdecl; external MyHTMLLib;
function mchar_async_crop_first_chars_without_cache (data : PChar; crop_len :
  QWord) : PChar; cdecl; external MyHTMLLib;
function mchar_async_get_size_by_data (const data : PChar) : QWord; cdecl;
  external MyHTMLLib;

// cache
function mchar_async_cache_init (cache : pmchar_async_cache_t) : mystatus_t;
  cdecl; external MyHTMLLib;
function mchar_async_cache_destroy (cache : pmchar_async_cache_t; self_destroy :
  Boolean) : pmchar_async_cache_t; cdecl; external MyHTMLLib;
procedure mchar_async_cache_clean (cache : pmchar_async_cache_t); cdecl;
  external MyHTMLLib;
procedure mchar_async_cache_add (cache : pmchar_async_cache_t; value : Pointer;
  size : QWord); cdecl; external MyHTMLLib;
function mchar_async_cache_delete (cache : pmchar_async_cache_t; size : QWord) :
  QWord; cdecl; external MyHTMLLib;

(*mycore/utils/mhash.h*********************************************************)

type
  ppmycore_utils_mhash_entry_t = ^pmycore_utils_mhash_entry_t;
  pmycore_utils_mhash_entry_t = ^mycore_utils_mhash_entry_t;
  mycore_utils_mhash_entry_t = record
    key : PChar;
    key_length : QWord;

    value : Pointer;

    next : pmycore_utils_mhash_entry_t;
  end;

  pmycore_utils_mhash_t = ^mycore_utils_mhash_t;
  mycore_utils_mhash_t = record
    mchar_obj : pmchar_async_t;
    mchar_node : QWord;

    table : ppmycore_utils_mhash_entry_t;
    table_size : QWord;
    table_length : QWord;

    table_max_depth : QWord;
  end;

function mycore_utils_mhash_create : pmycore_utils_mhash_t; cdecl;
  external MyHTMLLib;
function mycore_utils_mhash_init (mhash : pmycore_utils_mhash_t; table_size :
  QWord; depth : QWord) : mystatus_t; cdecl; external MyHTMLLib;
procedure mycore_utils_mhash_clean (mhash : pmycore_utils_mhash_t); cdecl;
  external MyHTMLLib;
function mycore_utils_mhash_destroy (mhash : pmycore_utils_mhash_t;
  self_destroy : Boolean) : pmycore_utils_mhash_t; cdecl; external MyHTMLLib;
function mycore_utils_mhash_create_entry (mhash : pmycore_utils_mhash_t;
  const key : PChar; key_size : QWord; value : Pointer) :
  pmycore_utils_mhash_entry_t; cdecl; external MyHTMLLib;
function mycore_utils_mhash_add (mhash : pmycore_utils_mhash_t; const key :
  PChar; key_size : QWord; value : Pointer) : pmycore_utils_mhash_entry_t;
  cdecl; external MyHTMLLib;
function mycore_utils_mhash_search (mhash : pmycore_utils_mhash_t; const key :
  PChar; key_size : QWord; value : Pointer) : pmycore_utils_mhash_entry_t;
  cdecl; external MyHTMLLib;
function mycore_utils_mhash_add_with_choice (mhash : pmycore_utils_mhash_t;
  const key : PChar; key_size : QWord) : pmycore_utils_mhash_entry_t; cdecl;
  external MyHTMLLib;
function mycore_utils_mhash_entry_by_id (mhash : pmycore_utils_mhash_t; id :
  QWord) : pmycore_utils_mhash_entry_t; cdecl; external MyHTMLLib;
function mycore_utils_mhash_get_table_size (mhash : pmycore_utils_mhash_t) :
  QWord; cdecl; external MyHTMLLib;
function mycore_utils_mhash_rebuld (mhash : pmycore_utils_mhash_t) :
  ppmycore_utils_mhash_entry_t; cdecl; external MyHTMLLib;

(*mycore/utils/mctree.h********************************************************)

 type
   pmctree_index_t = ^mctree_index_t;
   mctree_index_t = type QWord;

   pmctree_node_t = ^mctree_node_t;
   mctree_node_t = record
     str : PChar;
     str_size : QWord;
     value : Pointer;

     child_count : QWord;
     prev : mctree_index_t;
     next : mctree_index_t;
     child : mctree_index_t;
   end;

   pmctree_t = ^mctree_t;
   mctree_t = record
     nodes : pmctree_node_t;
     nodes_length : QWord;
     nodes_size : QWord;
     start_size : QWord;
   end;

   mctree_before_insert_f = procedure (const key : PChar; key_size : QWord;
     value : Pointer) of object;

function mctree_create (start_size : QWord) : pmctree_t; cdecl;
  external MyHTMLLib;
procedure mctree_clean (mctree : pmctree_t); cdecl; external MyHTMLLib;
function mctree_destroy (mctree : pmctree_t) : pmctree_t; cdecl;
  external MyHTMLLib;
function mctree_insert (mctree : pmctree_t; const key : PChar; key_size : QWord;
  value : Pointer; b_insert : mctree_before_insert_f) : mctree_index_t; cdecl;
  external MyHTMLLib;
function mctree_search (mctree : pmctree_t; const key : PChar; key_size : QWord)
  : mctree_index_t; cdecl; external MyHTMLLib;
function mctree_search_lowercase (mctree : pmctree_t; const key : PChar;
  key_size : QWord) : mctree_index_t; cdecl; external MyHTMLLib;

(*mycore/incoming.h************************************************************)

type
  ppmycore_incoming_buffer_t = ^pmycore_incoming_buffer_t;
  pmycore_incoming_buffer_t = ^mycore_incoming_buffer_t;
  mycore_incoming_buffer_t = record
    data : PChar;
    length : QWord; (* use of data *)
    size : QWord;   (* size of data *)
    offset : QWord; (* begin global offset *)

    prev : pmycore_incoming_buffer_t;
    next : pmycore_incoming_buffer_t;
  end;

function mycore_incoming_buffer_add (current : pmycore_incoming_buffer_t;
  mcobject : pmcobject_t; const html : PChar; html_size : QWord) :
  pmycore_incoming_buffer_t; cdecl external MyHTMLLib;
procedure mycore_incoming_buffer_clean (current : pmycore_incoming_buffer_t);
  cdecl; external MyHTMLLib;
function mycore_incoming_buffer_split (current : pmycore_incoming_buffer_t;
  mcobject : pmcobject_t; global_pos : QWord) : pmycore_incoming_buffer_t;
  cdecl; external MyHTMLLib;
function mycore_incoming_buffer_find_by_position (inc_buf :
  pmycore_incoming_buffer_t; start : QWord) : pmycore_incoming_buffer_t; cdecl;
  external MyHTMLLib;
function mycore_incoming_buffer_data (inc_buf : pmycore_incoming_buffer_t) :
  PChar; cdecl; external MyHTMLLib;
function mycore_incoming_buffer_length (inc_buf : pmycore_incoming_buffer_t) :
  QWord; cdecl; external MyHTMLLib;
function mycore_incoming_buffer_size (inc_buf : pmycore_incoming_buffer_t) :
  QWord; cdecl; external MyHTMLLib;
function mycore_incoming_buffer_offset (inc_buf : pmycore_incoming_buffer_t) :
  QWord; cdecl; external MyHTMLLib;
function mycore_incoming_buffer_relative_begin (inc_buf :
  pmycore_incoming_buffer_t; start : QWord) : QWord; cdecl; external MyHTMLLib;
function mycore_incoming_buffer_available_length (inc_buf :
  pmycore_incoming_buffer_t;  relative_begin : QWord; length : QWord) : QWord;
  cdecl; external MyHTMLLib;
function mycore_incoming_buffer_next (inc_buf : pmycore_incoming_buffer_t) :
  pmycore_incoming_buffer_t; cdecl; external MyHTMLLib;
function mycore_incoming_buffer_prev (inc_buf : pmycore_incoming_buffer_t) :
  pmycore_incoming_buffer_t; cdecl; external MyHTMLLib;
function mycore_incoming_buffer_convert_one_escape_to_code_point (inc_buf :
  ppmycore_incoming_buffer_t; relative_pos : PQword) : QWord; cdecl;
  external MyHTMLLib;
function mycore_incoming_buffer_escaped_case_cmp (inc_buf :
  ppmycore_incoming_buffer_t; const to_ : PChar; to_size : QWord; relative_pos :
  PQWord) : QWord; cdecl; external MyHTMLLib;

(*mycore/mystring.h************************************************************)

type
  pmycore_string_t = ^mycore_string_t;
  mycore_string_t = record
    data : PChar;
    size : QWord;
    length : QWord;

    mchar : pmchar_async_t;
    node_idx :  QWord;
  end;

  pmycore_string_raw_t = ^mycore_string_raw_t;
  mycore_string_raw_t = record
    data : PChar;
    size : QWord;
    length : QWord;
  end;

  mycore_string_index_t = type QWord;

function mycore_string_init (mchar : pmchar_async_t; node_idx : QWord; str :
  pmycore_string_t; size : QWord) : PChar; cdecl; external MyHTMLLib;
function mycore_string_realloc (str : pmycore_string_t; new_size : QWord) :
  PChar; cdecl; external MyHTMLLib;
procedure mycore_string_clean (str : pmycore_string_t); cdecl;
  external MyHTMLLib;
procedure mycore_string_clean_all (str : pmycore_string_t); cdecl;
  external MyHTMLLib;
function mycore_string_destroy (str : pmycore_string_t; destroy_obj : Boolean) :
  pmycore_string_raw_t; cdecl; external MyHTMLLib;

(* basic api *)
function mycore_string_data_alloc (mchar : pmchar_async_t; node_id : QWord;
  size : QWord) : PChar; cdecl; external MyHTMLLib;
function mycore_string_data_realloc (mchar : pmchar_async_t; node_id : QWord;
  data : PChar; len_to_copy : QWord; size : QWord) : PChar; cdecl;
  external MyHTMLLib;
procedure mycore_string_data_free (mchar : pmchar_async_t; node_id : QWord;
  data : PChar); cdecl; external MyHTMLLib;
function mycore_string_data (str : pmycore_string_t) : PChar; cdecl;
  external MyHTMLLib;
function mycore_string_length (str : pmycore_string_t) : QWord; cdecl;
  external MyHTMLLib;
function mycore_string_size (str : pmycore_string_t) : QWord; cdecl;
  external MyHTMLLib;
function mycore_string_data_set (str : pmycore_string_t; data : PChar) : PChar;
  cdecl; external MyHTMLLib;
function mycore_string_size_set ( str : pmycore_string_t; size : QWord) : QWord;
  cdecl; external MyHTMLLib;
function mycore_string_length_set (str : pmycore_string_t; length : QWord) :
  QWord; cdecl; external MyHTMLLib;

(* append *)
procedure mycore_string_append (str : pmycore_string_t; const data : PChar;
  length : QWord); cdecl; external MyHTMLLib;
procedure mycore_string_append_one (str : pmycore_string_t; const data : Char);
  cdecl; external MyHTMLLib;
procedure mycore_string_append_lowercase (str : pmycore_string_t; const data :
  PChar; length : QWord); cdecl; external MyHTMLLib;
procedure mycore_string_append_with_replacement_null_characters (str :
  pmycore_string_t; const buff : PChar; length : QWord); cdecl;
  external MyHTMLLib;
function mycore_string_raw_set_replacement_character (target : pmycore_string_t;
  position : QWord) : QWord; cdecl; external MyHTMLLib;

(* other functions *)
procedure mycore_string_copy (dest : pmycore_string_t; target :
  pmycore_string_t); cdecl; external MyHTMLLib;
function mycore_string_raw_copy (str1 : PChar; str2 : PChar; size : QWord) :
  QWord; cdecl; external MyHTMLLib;
procedure mycore_string_stay_only_whitespace (target : pmycore_string_t); cdecl;
  external MyHTMLLib;
function mycore_string_crop_whitespace_from_begin (target : pmycore_string_t) :
  QWord; cdecl; external MyHTMLLib;
function mycore_string_whitespace_from_begin (target : pmycore_string_t) :
  QWord; cdecl; external MyHTMLLib;

(*mycore/mythread.h************************************************************)

type
{$IFDEF MyCORE_BUILD_WITHOUT_THREADS}
  pmythread_t = ^mythread_t;
  mythread_t = record
    sys_last_error : Integer;
  end;
{$ELSE}
  ppmythread_t = ^pmythread_t;
  pmythread_t = ^mythread_t;

  pmythread_entry_t = ^mythread_entry_t;

  mythread_callback_before_entry_join_f = procedure (mythread : pmythread_t;
    entry : pmythread_entry_t; ctx : Pointer) of object;
  mythread_process_f = function (arg : Pointer) : Pointer of object;
  mythread_work_f = procedure (thread_id : mythread_id_t; arg : Pointer) of
    object;

  pmythread_thread_opt_t = ^mythread_thread_opt_t;
  mythread_thread_opt_t = (
    MyTHREAD_OPT_UNDEF                                                  = $0000,
    MyTHREAD_OPT_WAIT                                                   = $0001,
    MyTHREAD_OPT_QUIT                                                   = $0002,
    MyTHREAD_OPT_STOP                                                   = $0004,
    MyTHREAD_OPT_DONE                                                   = $0008
  );

  pmythread_type_t = ^mythread_type_t;
  mythread_type_t = (
    MyTHREAD_TYPE_STREAM                                                = $0000,
    MyTHREAD_TYPE_BATCH                                                 = $0001
  );

  (* thread *)
  pmythread_context_t = ^mythread_context_t;
  mythread_context_t = record
    id : mythread_id_t;
    func : mythread_work_f;

    count : QWord;
    opt : mythread_thread_opt_t;

    status : mystatus_t;

    mutex : Pointer;
    timespec : Pointer;
    mythread : pmythread_t;
  end;

  mythread_entry_t = record
    thread : Pointer;

    context : mythread_context_t;
    process_func : mythread_process_f;
  end;

  mythread_t = record
    entries : pmythread_entry_t;
    entries_length : QWord;
    entries_size : QWord;
    id_increase : QWord;

    context : Pointer;
    attr : Pointer;
    timespec : Pointer;

    sys_last_error : Integer;

    thread_type : mythread_type_t;
    opt : mythread_thread_opt_t;
  end;

function mythread_create : pmythread_t; cdecl; external MyHTMLLib;
function mythread_init (mythread : pmythread_t; thread_type : mythread_type_t;
  threads_count : QWord; id_increase : QWord) : mystatus_t; cdecl;
  external MyHTMLLib;
procedure mythread_clean (mythread : pmythread_t); cdecl; external MyHTMLLib;
function mythread_destroy (mythread : pmythread_t; before_join :
  mythread_callback_before_entry_join_f; ctx : Pointer; self_destroy : Boolean)
  : pmythread_t; cdecl; external MyHTMLLib;
function mythread_increase_id_by_entry_id (mythread : pmythread_t; thread_id :
  mythread_id_t) : mythread_id_t; cdecl; external MyHTMLLib;

(* set for all threads *)
function mythread_join (mythread : pmythread_t; before_join :
  mythread_callback_before_entry_join_f; ctx : Pointer) : mystatus_t; cdecl;
  external MyHTMLLib;
function mythread_quit (mythread : pmythread_t; before_join :
  mythread_callback_before_entry_join_f; ctx : Pointer) : mystatus_t; cdecl;
  external MyHTMLLib;
function mythread_stop (mythread : pmythread_t) : mystatus_t; cdecl;
  external MyHTMLLib;
function mythread_resume (mythread : pmythread_t; send_opt :
  mythread_thread_opt_t) : mystatus_t; cdecl; external MyHTMLLib;
function mythread_suspend (mythread : pmythread_t) : mystatus_t; cdecl;
  external MyHTMLLib;
function mythread_check_status (mythread : pmythread_t) : mystatus_t; cdecl;
  external MyHTMLLib;
function mythread_option (mythread : pmythread_t) : mythread_thread_opt_t;
  cdecl; external MyHTMLLib;
procedure mythread_option_set (mythread : pmythread_t; opt :
  mythread_thread_opt_t); cdecl; external MyHTMLLib;

(* Entries *)
function mythread_entry_create (mythread : pmythread_t; process_func :
  mythread_process_f; func : mythread_work_f; opt : mythread_thread_opt_t) :
  mystatus_t; cdecl; external MyHTMLLib;
function mythread_entry_join (entry : pmythread_entry_t; before_join :
  mythread_callback_before_entry_join_f; ctx : Pointer) : mystatus_t; cdecl;
  external MyHTMLLib;
function mythread_entry_quit (entry : pmythread_entry_t; before_join :
  mythread_callback_before_entry_join_f; ctx : Pointer) : mystatus_t; cdecl;
  external MyHTMLLib;
function mythread_entry_stop (entry : pmythread_entry_t) : mystatus_t; cdecl;
  external MyHTMLLib;
function mythread_entry_resume (entry : pmythread_entry_t; send_opt :
  mythread_thread_opt_t) : mystatus_t; cdecl; external MyHTMLLib;
function mythread_entry_suspend (entry : pmythread_entry_t) : mystatus_t; cdecl;
  external MyHTMLLib;
function mythread_entry_status (entry : pmythread_entry_t) : mystatus_t; cdecl;
  external MyHTMLLib;
function mythread_entry_mythread (entry : pmythread_entry_t) : pmythread_t;
  cdecl; external MyHTMLLib;

(* API for ports *)
function mythread_thread_create (mythread : pmythread_t; process_func :
  mythread_process_f; ctx : Pointer) : Pointer; cdecl; external MyHTMLLib;
function mythread_thread_join (mythread : pmythread_t; thread : Pointer) :
  mystatus_t; cdecl; external MyHTMLLib;
function mythread_thread_cancel (mythread : pmythread_t; thread : Pointer) :
  mystatus_t; cdecl; external MyHTMLLib;
function mythread_thread_destroy (mythread : pmythread_t; thread : Pointer) :
  mystatus_t; cdecl; external MyHTMLLib;
function mythread_thread_attr_init (mythread : pmythread_t) : Pointer; cdecl;
  external MyHTMLLib;
procedure mythread_thread_attr_clean (mythread : pmythread_t; attr : Pointer);
  cdecl; external MyHTMLLib;
procedure mythread_thread_attr_destroy (mythread : pmythread_t; attr : Pointer);
  cdecl; external MyHTMLLib;
function mythread_mutex_create (mythread : pmythread_t) : Pointer; cdecl;
  external MyHTMLLib;
function mythread_mutex_post (mythread : pmythread_t; mutex : Pointer) :
  mystatus_t; cdecl; external MyHTMLLib;
function mythread_mutex_wait (mythread : pmythread_t; mutex : Pointer) :
  mystatus_t; cdecl; external MyHTMLLib;
procedure mythread_mutex_close (mythread : pmythread_t; mutex : Pointer); cdecl;
  external MyHTMLLib;
function mythread_nanosleep_create (mythread : pmythread_t) : Pointer; cdecl;
  external MyHTMLLib;
procedure mythread_nanosleep_clean (timespec : Pointer); cdecl;
  external MyHTMLLib;
procedure mythread_nanosleep_destroy (timespec : Pointer); cdecl;
  external MyHTMLLib;
function mythread_nanosleep_sleep (timespec : Pointer) : mystatus_t; cdecl;
  external MyHTMLLib;

(* callback *)
procedure mythread_callback_quit (mythread : pmythread_t; entry :
  pmythread_entry_t; ctx : Pointer); cdecl; external MyHTMLLib;
{$ENDIF}{MyCORE_BUILD_WITHOUT_THREADS}

(*mycore/thread_queue.h********************************************************)

type
  (* queue *)
  ppmythread_queue_node_t = ^pmythread_queue_node_t;
  pmythread_queue_node_t = ^mythread_queue_node_t;
  mythread_queue_node_t = record
    context : Pointer;
    args : Pointer;

    prev : pmythread_queue_node_t;
  end;

  pmythread_queue_t = ^mythread_queue_t;
  mythread_queue_t = record
    nodes : ppmythread_queue_node_t;

    nodes_pos : QWord;
    nodes_pos_size : QWord;
    nodes_length : QWord;

    nodes_uses : QWord;
    nodes_size : QWord;
    nodes_root : QWord;
  end;

  pmythread_queue_thread_param_t = ^mythread_queue_thread_param_t;
  mythread_queue_thread_param_t = record
    use : QWord;
  end;

  pmythread_queue_list_entry_t = ^mythread_queue_list_entry_t;
  mythread_queue_list_entry_t = record
    queue : pmythread_queue_t;
    thread_param : pmythread_queue_thread_param_t;
    thread_param_size : QWord;

    next : pmythread_queue_list_entry_t;
    prev : pmythread_queue_list_entry_t;
  end;

  pmythread_queue_list_t = ^mythread_queue_list_t;
  mythread_queue_list_t = record
    first : pmythread_queue_list_entry_t;
    last : pmythread_queue_list_entry_t;

    count : QWord;
  end;

function mythread_queue_create : pmythread_queue_t; cdecl; external MyHTMLLib;
function mythread_queue_init (queue : pmythread_queue_t; size : QWord) :
  mystatus_t; cdecl; external MyHTMLLib;
procedure mythread_queue_clean (queue : pmythread_queue_t); cdecl;
  external MyHTMLLib;
function mythread_queue_destroy (token : pmythread_queue_t) : pmythread_queue_t;
  cdecl; external MyHTMLLib;
procedure mythread_queue_node_clean (qnode : pmythread_queue_node_t); cdecl;
  external MyHTMLLib;
function mythread_queue_count_used_node (queue : pmythread_queue_t) : QWord;
  cdecl; external MyHTMLLib;
function mythread_queue_get_first_node (queue : pmythread_queue_t) :
  pmythread_queue_node_t; cdecl; external MyHTMLLib;
function mythread_queue_get_prev_node (qnode : pmythread_queue_node_t) :
  pmythread_queue_node_t; cdecl; external MyHTMLLib;
function mythread_queue_get_current_node (queue : pmythread_queue_t) :
  pmythread_queue_node_t; cdecl; external MyHTMLLib;
function mythread_queue_node_malloc (mythread : pmythread_t; queue :
  pmythread_queue_t; status : pmystatus_t) : pmythread_queue_node_t; cdecl;
  external MyHTMLLib;
function mythread_queue_node_malloc_limit (mythread : pmythread_t; queue :
  pmythread_queue_t; limit : QWord; status : pmystatus_t) :
  pmythread_queue_node_t; cdecl; external MyHTMLLib;
{$IFNDEF MyCORE_BUILD_WITHOUT_THREADS}
function mythread_queue_node_malloc_round (mythread : pmythread_t; entry :
  pmythread_queue_list_entry_t) : pmythread_queue_node_t; cdecl;
  external MyHTMLLib;
function mythread_queue_list_create (status : pmystatus_t) :
  pmythread_queue_list_t; cdecl; external MyHTMLLib;
procedure mythread_queue_list_destroy (queue_list : pmythread_queue_list_t);
  cdecl; external MyHTMLLib;
function mythread_queue_list_get_count (queue_list : pmythread_queue_list_t) :
  QWord; cdecl; external MyHTMLLib;
procedure mythread_queue_list_wait_for_done (mythread : pmythread_t;
  queue_list : pmythread_queue_list_t); cdecl; external MyHTMLLib;
function mythread_queue_list_see_for_done (mythread : pmythread_t;
  queue_list : pmythread_queue_list_t) : Boolean; cdecl; external MyHTMLLib;
function mythread_queue_list_see_for_done_by_thread (mythread : pmythread_t;
  queue_list : pmythread_queue_list_t; thread_id : mythread_id_t) : Boolean;
  cdecl; external MyHTMLLib;
function mythread_queue_list_entry_push (mythread_list : ppmythread_t;
  list_size : QWord; queue_list : pmythread_queue_list_t; queue :
  pmythread_queue_t; thread_param_size : QWord; status : pmystatus_t) :
  pmythread_queue_list_t; cdecl; external MyHTMLLib;
function mythread_queue_list_entry_delete (mythread_list : ppmythread_t;
  list_size : QWord; queue_list : pmythread_queue_list_t; entry :
  pmythread_queue_list_entry_t; destroy_queue : Boolean) :
  pmythread_queue_list_entry_t; cdecl; external MyHTMLLib;
procedure mythread_queue_list_entry_clean (entry :
  pmythread_queue_list_entry_t); cdecl; external MyHTMLLib;
procedure mythread_queue_list_entry_wait_for_done (mythread : pmythread_t;
  entry : pmythread_queue_list_entry_t); cdecl; external MyHTMLLib;
procedure mythread_queue_list_entry_make_batch (mythread : pmythread_t;
  entry : pmythread_queue_list_entry_t); cdecl; external MyHTMLLib;
procedure mythread_queue_list_entry_make_stream (mythread : pmythread_t;
  entry : pmythread_queue_list_entry_t); cdecl; external MyHTMLLib;
{$ENDIF}{MyCORE_BUILD_WITHOUT_THREADS}

(*mycore/myosi.h***************************************************************)

type
  mycore_status_t = (
    MyCORE_STATUS_OK                                                    = $0000,
    MyCORE_STATUS_ERROR                                                 = $0001,
    MyCORE_STATUS_ERROR_MEMORY_ALLOCATION                               = $0002,
    MyCORE_STATUS_THREAD_ERROR_MEMORY_ALLOCATION                        = $0009,
    MyCORE_STATUS_THREAD_ERROR_LIST_INIT                                = $000a,
    MyCORE_STATUS_THREAD_ERROR_ATTR_MALLOC                              = $000b,
    MyCORE_STATUS_THREAD_ERROR_ATTR_INIT                                = $000c,
    MyCORE_STATUS_THREAD_ERROR_ATTR_SET                                 = $000d,
    MyCORE_STATUS_THREAD_ERROR_ATTR_DESTROY                             = $000e,
    MyCORE_STATUS_THREAD_ERROR_NO_SLOTS                                 = $000f,
    MyCORE_STATUS_THREAD_ERROR_BATCH_INIT                               = $0010,
    MyCORE_STATUS_THREAD_ERROR_WORKER_MALLOC                            = $0011,
    MyCORE_STATUS_THREAD_ERROR_WORKER_SEM_CREATE                        = $0012,
    MyCORE_STATUS_THREAD_ERROR_WORKER_THREAD_CREATE                     = $0013,
    MyCORE_STATUS_THREAD_ERROR_MASTER_THREAD_CREATE                     = $0014,
    MyCORE_STATUS_THREAD_ERROR_SEM_PREFIX_MALLOC                        = $0032,
    MyCORE_STATUS_THREAD_ERROR_SEM_CREATE                               = $0033,
    MyCORE_STATUS_THREAD_ERROR_QUEUE_MALLOC                             = $003c,
    MyCORE_STATUS_THREAD_ERROR_QUEUE_NODES_MALLOC                       = $003d,
    MyCORE_STATUS_THREAD_ERROR_QUEUE_NODE_MALLOC                        = $003e,
    MyCORE_STATUS_THREAD_ERROR_MUTEX_MALLOC                             = $0046,
    MyCORE_STATUS_THREAD_ERROR_MUTEX_INIT                               = $0047,
    MyCORE_STATUS_THREAD_ERROR_MUTEX_LOCK                               = $0048,
    MyCORE_STATUS_THREAD_ERROR_MUTEX_UNLOCK                             = $0049,
    MyCORE_STATUS_PERF_ERROR_COMPILED_WITHOUT_PERF                      = $0050,
    MyCORE_STATUS_PERF_ERROR_FIND_CPU_CLOCK                             = $0051,
    MyCORE_STATUS_MCOBJECT_ERROR_CACHE_CREATE                           = $0055,
    MyCORE_STATUS_MCOBJECT_ERROR_CHUNK_CREATE                           = $0056,
    MyCORE_STATUS_MCOBJECT_ERROR_CHUNK_INIT                             = $0057,
    MyCORE_STATUS_MCOBJECT_ERROR_CACHE_REALLOC                          = $0058,
    MyCORE_STATUS_ASYNC_ERROR_LOCK                                      = $0060,
    MyCORE_STATUS_ASYNC_ERROR_UNLOCK                                    = $0061,
    MyCORE_STATUS_ERROR_NO_FREE_SLOT                                    = $0062
  );

  mycore_callback_serialize_f = function (const buffer : PChar; size : QWord;
    ctx : Pointer) : mystatus_t of object;

function mycore_malloc (size : QWord) : Pointer; cdecl; external MyHTMLLib;
function mycore_realloc (dst : Pointer; size : QWord) : Pointer; cdecl;
  external MyHTMLLib;
function mycore_calloc (num : QWord; size : QWord) : Pointer; cdecl;
  external MyHTMLLib;
function mycore_free (dst : Pointer) : Pointer; cdecl; external MyHTMLLib;

(* io *)
function mycore_fopen (const filename : PChar; const mode : PChar) : Pointer;
  cdecl; external MyHTMLLib;
function mycore_fclose (stream : Pointer) : Integer; cdecl; external MyHTMLLib;
function mycore_fread (buffer : Pointer; size : QWord; count : QWord; stream :
  Pointer) : QWord; cdecl; external MyHTMLLib;
function mycore_fwrite (const buffer : Pointer; size : QWord; count : QWord;
  stream : Pointer) : QWord; cdecl; external MyHTMLLib;
function mycore_fflush (stream : Pointer) : Integer; cdecl; external MyHTMLLib;
function mycore_fseek (stream : Pointer; offset : Longint; origin : Integer) :
  Integer; cdecl; external MyHTMLLib;
function mycore_ftell (stream : Pointer) : Longint; cdecl; external MyHTMLLib;
function mycore_ferror (stream : Pointer) : Integer; cdecl; external MyHTMLLib;
procedure mycore_setbuf (stream : Pointer; buffer : PChar); cdecl;
  external MyHTMLLib;

(*mycore/perf.h****************************************************************)

{$IFDEF MyHTML_WITH_PERF}
function mycore_perf_create : Pointer; cdecl; external MyHTMLLib;
procedure mycore_perf_clean (perf : Pointer); cdecl; external MyHTMLLib;
procedure mycore_perf_destroy (perf : Pointer); cdecl; external MyHTMLLib;
function mycore_perf_begin (perf : Pointer) : mycore_status_t; cdecl;
  external MyHTMLLib;
function mycore_perf_end (perf : Pointer) : mycore_status_t; cdecl;
  external MyHTMLLib;
function mycore_pref_in_sec (perf : Pointer) : Double; cdecl;
  external MyHTMLLib;
function mycore_perf_clock : QWord; cdecl; external MyHTMLLib;
function mycore_perf_frequency : QWord; cdecl; external MyHTMLLib;
{$ENDIF}{MyHTML_WITH_PERF}

(*myencoding/myosi.h***********************************************************)

type
  (* encoding *)
  pmyencoding_t = ^myencoding_t;
  myencoding_t = (
    MyENCODING_DEFAULT                                                  = $0000,
    MyENCODING_UTF_8                                                    = $0000{%H-},
   {MyENCODING_AUTO                                                     = $0001,}
    MyENCODING_NOT_DETERMINED                                           = $0002,
    MyENCODING_UTF_16LE                                                 = $0004,
    MyENCODING_UTF_16BE                                                 = $0005,
    MyENCODING_X_USER_DEFINED                                           = $0006,
    MyENCODING_BIG5                                                     = $0007,
    MyENCODING_EUC_JP                                                   = $0008,
    MyENCODING_EUC_KR                                                   = $0009,
    MyENCODING_GB18030                                                  = $000a,
    MyENCODING_GBK                                                      = $000b,
    MyENCODING_IBM866                                                   = $000c,
    MyENCODING_ISO_2022_JP                                              = $000d,
    MyENCODING_ISO_8859_10                                              = $000e,
    MyENCODING_ISO_8859_13                                              = $000f,
    MyENCODING_ISO_8859_14                                              = $0010,
    MyENCODING_ISO_8859_15                                              = $0011,
    MyENCODING_ISO_8859_16                                              = $0012,
    MyENCODING_ISO_8859_2                                               = $0013,
    MyENCODING_ISO_8859_3                                               = $0014,
    MyENCODING_ISO_8859_4                                               = $0015,
    MyENCODING_ISO_8859_5                                               = $0016,
    MyENCODING_ISO_8859_6                                               = $0017,
    MyENCODING_ISO_8859_7                                               = $0018,
    MyENCODING_ISO_8859_8                                               = $0019,
    MyENCODING_ISO_8859_8_I                                             = $001a,
    MyENCODING_KOI8_R                                                   = $001b,
    MyENCODING_KOI8_U                                                   = $001c,
    MyENCODING_MACINTOSH                                                = $001d,
    MyENCODING_SHIFT_JIS                                                = $001e,
    MyENCODING_WINDOWS_1250                                             = $001f,
    MyENCODING_WINDOWS_1251                                             = $0020,
    MyENCODING_WINDOWS_1252                                             = $0021,
    MyENCODING_WINDOWS_1253                                             = $0022,
    MyENCODING_WINDOWS_1254                                             = $0023,
    MyENCODING_WINDOWS_1255                                             = $0024,
    MyENCODING_WINDOWS_1256                                             = $0025,
    MyENCODING_WINDOWS_1257                                             = $0026,
    MyENCODING_WINDOWS_1258                                             = $0027,
    MyENCODING_WINDOWS_874                                              = $0028,
    MyENCODING_X_MAC_CYRILLIC                                           = $0029,

    MyENCODING_LAST_ENTRY                                               = $002a
  );

  myencoding_status_t = (
    MyENCODING_STATUS_OK                                                = $0000,
    MyENCODING_STATUS_ERROR                                             = $0001,
    MyENCODING_STATUS_CONTINUE                                          = $0002,
    MyENCODING_STATUS_DONE                                              = $0004
  );

(*myencoding/encoding.h********************************************************)

type
  pmyencoding_result_t = ^myencoding_result_t;
  myencoding_result_t = record
    first : Cardinal;
    second : Cardinal;
    third : Cardinal;
    result : Cardinal;
    result_aux : Cardinal;
    flag : Cardinal;
  end;

  pmyencoding_trigram_t = ^myencoding_trigram_t;
  myencoding_trigram_t = record
    trigram : array [0 .. 2] of Byte;
    value : QWord;
  end;

  pmyencoding_trigram_result_t = ^myencoding_trigram_result_t;
  myencoding_trigram_result_t = record
    count : QWord;
    vaule : QWord;
  end;

  pmyencoding_unicode_result_t = ^myencoding_unicode_result_t;
  myencoding_unicode_result_t = record
    count_ascii : QWord;
    count_good : QWord;
    count_bad : QWord;
  end;

  pmyencoding_detect_name_entry_t = ^myencoding_detect_name_entry_t;
  myencoding_detect_name_entry_t = record
    name : PChar;
    name_length : QWord;
    label_name : PChar;
    label_length : QWord;

    encoding : myencoding_t;

    next : QWord;
    curr : QWord;
  end;

  pmyencoding_detect_attr_t = ^myencoding_detect_attr_t;
  myencoding_detect_attr_t = record
    key_begin : QWord;
    key_length : QWord;
    value_begin : QWord;
    value_length : QWord;

    next : pmyencoding_detect_attr_t;
  end;

  pmyencoding_entry_name_index_t = ^myencoding_entry_name_index_t;
  myencoding_entry_name_index_t = record
    name : PChar;
    length : QWord;
  end;

  myencoding_custom_f = function (const data : Byte; res : pmyencoding_result_t)
    : myencoding_status_t of object;

function myencoding_get_function_by_id (idx : myencoding_t) :
  myencoding_custom_f; cdecl; external MyHTMLLib;
function myencoding_decode_utf_8 (const data : Byte; res : pmyencoding_result_t)
  : myencoding_status_t; cdecl; external MyHTMLLib;
function myencoding_decode_ibm866 (const data : Byte; res :
  pmyencoding_result_t) : myencoding_status_t; cdecl; external MyHTMLLib;
function myencoding_decode_iso_8859_2 (const data : Byte; res :
  pmyencoding_result_t) : myencoding_status_t; cdecl; external MyHTMLLib;
function myencoding_decode_iso_8859_3 (const data : Byte; res :
  pmyencoding_result_t) : myencoding_status_t; cdecl; external MyHTMLLib;
function myencoding_decode_iso_8859_4 (const data : Byte; res :
  pmyencoding_result_t) : myencoding_status_t; cdecl; external MyHTMLLib;
function myencoding_decode_iso_8859_5 (const data : Byte; res :
  pmyencoding_result_t) : myencoding_status_t; cdecl; external MyHTMLLib;
function myencoding_decode_iso_8859_6 (const data : Byte; res :
  pmyencoding_result_t) : myencoding_status_t; cdecl; external MyHTMLLib;
function myencoding_decode_iso_8859_7 (const data : Byte; res :
  pmyencoding_result_t) : myencoding_status_t; cdecl; external MyHTMLLib;
function myencoding_decode_iso_8859_8 (const data : Byte; res :
  pmyencoding_result_t) : myencoding_status_t; cdecl; external MyHTMLLib;
function myencoding_decode_iso_8859_8_i (const data : Byte; res :
  pmyencoding_result_t) : myencoding_status_t; cdecl; external MyHTMLLib;
function myencoding_decode_iso_8859_10 (const data : Byte; res :
  pmyencoding_result_t) : myencoding_status_t; cdecl; external MyHTMLLib;
function myencoding_decode_iso_8859_13 (const data : Byte; res :
  pmyencoding_result_t) : myencoding_status_t; cdecl; external MyHTMLLib;
function myencoding_decode_iso_8859_14 (const data : Byte; res :
  pmyencoding_result_t) : myencoding_status_t; cdecl; external MyHTMLLib;
function myencoding_decode_iso_8859_15 (const data : Byte; res :
  pmyencoding_result_t) : myencoding_status_t; cdecl; external MyHTMLLib;
function myencoding_decode_iso_8859_16 (const data : Byte; res :
  pmyencoding_result_t) : myencoding_status_t; cdecl; external MyHTMLLib;
function myencoding_decode_koi8_r (const data : Byte; res :
  pmyencoding_result_t) : myencoding_status_t; cdecl; external MyHTMLLib;
function myencoding_decode_koi8_u (const data : Byte; res :
  pmyencoding_result_t) : myencoding_status_t; cdecl; external MyHTMLLib;
function myencoding_decode_macintosh (const data : Byte; res :
  pmyencoding_result_t) : myencoding_status_t; cdecl; external MyHTMLLib;
function myencoding_decode_windows_874 (const data : Byte; res :
  pmyencoding_result_t) : myencoding_status_t; cdecl; external MyHTMLLib;
function myencoding_decode_windows_1250 (const data : Byte; res :
  pmyencoding_result_t) : myencoding_status_t; cdecl; external MyHTMLLib;
function myencoding_decode_windows_1251 (const data : Byte; res :
  pmyencoding_result_t) : myencoding_status_t; cdecl; external MyHTMLLib;
function myencoding_decode_windows_1252 (const data : Byte; res :
  pmyencoding_result_t) : myencoding_status_t; cdecl; external MyHTMLLib;
function myencoding_decode_windows_1253 (const data : Byte; res :
  pmyencoding_result_t) : myencoding_status_t; cdecl; external MyHTMLLib;
function myencoding_decode_windows_1254 (const data : Byte; res :
  pmyencoding_result_t) : myencoding_status_t; cdecl; external MyHTMLLib;
function myencoding_decode_windows_1255 (const data : Byte; res :
  pmyencoding_result_t) : myencoding_status_t; cdecl; external MyHTMLLib;
function myencoding_decode_windows_1256 (const data : Byte; res :
  pmyencoding_result_t) : myencoding_status_t; cdecl; external MyHTMLLib;
function myencoding_decode_windows_1257 (const data : Byte; res :
  pmyencoding_result_t) : myencoding_status_t; cdecl; external MyHTMLLib;
function myencoding_decode_windows_1258 (const data : Byte; res :
  pmyencoding_result_t) : myencoding_status_t; cdecl; external MyHTMLLib;
function myencoding_decode_x_mac_cyrillic (const data : Byte; res :
  pmyencoding_result_t) : myencoding_status_t; cdecl; external MyHTMLLib;
function myencoding_decode_gbk (const data : Byte; res : pmyencoding_result_t)
  : myencoding_status_t; cdecl; external MyHTMLLib;
function myencoding_decode_gb18030 (const data : Byte; res :
  pmyencoding_result_t) : myencoding_status_t; cdecl; external MyHTMLLib;
function myencoding_decode_big5 (const data : Byte; res :
  pmyencoding_result_t) : myencoding_status_t; cdecl; external MyHTMLLib;
function myencoding_decode_euc_jp (const data : Byte; res :
  pmyencoding_result_t) : myencoding_status_t; cdecl; external MyHTMLLib;
function myencoding_decode_iso_2022_jp (const data : Byte; res :
  pmyencoding_result_t) : myencoding_status_t; cdecl; external MyHTMLLib;
function myencoding_decode_shift_jis (const data : Byte; res :
  pmyencoding_result_t) : myencoding_status_t; cdecl; external MyHTMLLib;
function myencoding_decode_euc_kr (const data : Byte; res :
  pmyencoding_result_t) : myencoding_status_t; cdecl; external MyHTMLLib;
function myencoding_decode_shared_utf_16 (const data : Byte; res :
  pmyencoding_result_t) : myencoding_status_t; cdecl; external MyHTMLLib;
function myencoding_decode_utf_16be (const data : Byte; res :
  pmyencoding_result_t) : myencoding_status_t; cdecl; external MyHTMLLib;
function myencoding_decode_utf_16le (const data : Byte; res :
  pmyencoding_result_t) : myencoding_status_t; cdecl; external MyHTMLLib;
function myencoding_decode_x_user_defined (const data : Byte; res :
  pmyencoding_result_t) : myencoding_status_t; cdecl; external MyHTMLLib;
function myencoding_codepoint_ascii_length (codepoint : QWord) :  QWord; cdecl;
  external MyHTMLLib;
function myencoding_ascii_utf_8_length (const data : Byte) : QWord; cdecl;
  external MyHTMLLib;
function myencoding_codepoint_to_ascii_utf_8 (codepoint : QWord; data : PChar) :
  QWord; cdecl; external MyHTMLLib;
function myencoding_codepoint_to_lowercase_ascii_utf_8 (codepoint : QWord;
  data : PChar) : QWord; cdecl; external MyHTMLLib;
function myencoding_codepoint_to_ascii_utf_16 (codepoint : QWord; data : PChar)
  : QWord; cdecl; external MyHTMLLib;
function myencoding_ascii_utf_8_to_codepoint (const data : PByte; codepoint :
  PQWord) : QWord; cdecl; external MyHTMLLib;
procedure myencoding_result_clean (res : pmyencoding_result_t); cdecl;
  external MyHTMLLib;
function myencoding_detect (const text : PChar; length : QWord; encoding :
  pmyencoding_t) : Boolean; cdecl; external MyHTMLLib;
function myencoding_detect_russian (const text : PChar; length : QWord;
  encoding : pmyencoding_t) : Boolean; cdecl; external MyHTMLLib;
function myencoding_detect_unicode (const text : PChar; length : QWord;
  encoding : pmyencoding_t) : Boolean; cdecl; external MyHTMLLib;
function myencoding_detect_bom (const text : PChar; length : QWord;
  encoding : pmyencoding_t) : Boolean; cdecl; external MyHTMLLib;
function myencoding_detect_and_cut_bom (const text : PChar; length : PChar;
  encoding : pmyencoding_t; const new_text : PPChar; new_size : QWord) :
  Boolean; cdecl; external MyHTMLLib;
function myencoding_convert_to_ascii_utf_8 (raw_str : pmycore_string_raw_t;
  const buff : PChar; length : QWord; encoding : myencoding_t) : QWord; cdecl;
  external MyHTMLLib;
function myencoding_name_entry_by_name (const name : PChar; length : QWord) :
  pmyencoding_detect_name_entry_t; cdecl; external MyHTMLLib;
function myencoding_by_name (const name : PChar; length : QWord; encoding :
  pmyencoding_t) : Boolean; cdecl; external MyHTMLLib;
function myencoding_name_by_id (encoding : myencoding_t; length : PQWord) :
  PChar; cdecl; external MyHTMLLib;
function myencoding_extracting_character_encoding_from_charset (const data :
  PChar; data_size : QWord; encoding : pmyencoding_t) : Boolean; cdecl;
  external MyHTMLLib;
function myencoding_prescan_stream_to_determine_encoding (const data : PChar;
  data_size : QWord) : myencoding_t; cdecl; external MyHTMLLib;
function myencoding_extracting_character_encoding_from_charset_with_found (
  const data : PChar; data_size : QWord; encoding : pmyencoding_t; const found :
  PPChar; found_length : PQWord) : Boolean; cdecl; external MyHTMLLib;
function myencoding_prescan_stream_to_determine_encoding_with_found (
  const data : PChar; data_size : QWord; const found : PPChar; found_length :
  PQWord) : myencoding_t; cdecl; external MyHTMLLib;

(*myencoding/mystring.h********************************************************)

procedure myencoding_string_append (str : pmycore_string_t; const buff :
  PChar; length : QWord; encoding : myencoding_t); cdecl; external MyHTMLLib;

(* append with convert encoding *)
procedure myencoding_string_append_chunk (str : pmycore_string_t; res :
  pmyencoding_result_t; const buff : PChar; length : QWord; encoding :
  myencoding_t); cdecl; external MyHTMLLib;
procedure myencoding_string_append_one (str : pmycore_string_t; res :
  pmyencoding_result_t; const data : Char; encoding : myencoding_t); cdecl;
  external MyHTMLLib;

(* append with convert encoding lowercase *)
procedure myencoding_string_append_lowercase_ascii (str : pmycore_string_t;
  const buff : PChar; length : QWord; encoding : myencoding_t); cdecl;
  external MyHTMLLib;
procedure myencoding_string_append_chunk_lowercase_ascii (str :
  pmycore_string_t; res : pmyencoding_result_t; const buff : PChar; length :
  QWord; encoding : myencoding_t); cdecl; external MyHTMLLib;

(*myhtml/tree.h****************************************************************)

type
  pmyhtml_tree_t = ^myhtml_tree_t;
  pmyhtml_tree_node_t = ^myhtml_tree_node_t;
  pmyhtml_token_node_t = ^myhtml_token_node_t;

(*myhtml/api.h*****************************************************************)

type
  pmyhtml_t = ^myhtml_t;
  pmyhtml_token_t = ^myhtml_token_t;
  pmyhtml_tag_t = ^myhtml_tag_t;

  pmyhtml_tag_id_t = ^myhtml_tag_id_t;
  myhtml_tag_id_t = type QWord;

  pmyhtml_tags = ^myhtml_tags;
  myhtml_tags = (
    MyHTML_TAG__UNDEF                                                   = $0000,
    MyHTML_TAG__TEXT                                                    = $0001,
    MyHTML_TAG__COMMENT                                                 = $0002,
    MyHTML_TAG__DOCTYPE                                                 = $0003,
    MyHTML_TAG_A                                                        = $0004,
    MyHTML_TAG_ABBR                                                     = $0005,
    MyHTML_TAG_ACRONYM                                                  = $0006,
    MyHTML_TAG_ADDRESS                                                  = $0007,
    MyHTML_TAG_ANNOTATION_XML                                           = $0008,
    MyHTML_TAG_APPLET                                                   = $0009,
    MyHTML_TAG_AREA                                                     = $000a,
    MyHTML_TAG_ARTICLE                                                  = $000b,
    MyHTML_TAG_ASIDE                                                    = $000c,
    MyHTML_TAG_AUDIO                                                    = $000d,
    MyHTML_TAG_B                                                        = $000e,
    MyHTML_TAG_BASE                                                     = $000f,
    MyHTML_TAG_BASEFONT                                                 = $0010,
    MyHTML_TAG_BDI                                                      = $0011,
    MyHTML_TAG_BDO                                                      = $0012,
    MyHTML_TAG_BGSOUND                                                  = $0013,
    MyHTML_TAG_BIG                                                      = $0014,
    MyHTML_TAG_BLINK                                                    = $0015,
    MyHTML_TAG_BLOCKQUOTE                                               = $0016,
    MyHTML_TAG_BODY                                                     = $0017,
    MyHTML_TAG_BR                                                       = $0018,
    MyHTML_TAG_BUTTON                                                   = $0019,
    MyHTML_TAG_CANVAS                                                   = $001a,
    MyHTML_TAG_CAPTION                                                  = $001b,
    MyHTML_TAG_CENTER                                                   = $001c,
    MyHTML_TAG_CITE                                                     = $001d,
    MyHTML_TAG_CODE                                                     = $001e,
    MyHTML_TAG_COL                                                      = $001f,
    MyHTML_TAG_COLGROUP                                                 = $0020,
    MyHTML_TAG_COMMAND                                                  = $0021,
    MyHTML_TAG_COMMENT                                                  = $0022,
    MyHTML_TAG_DATALIST                                                 = $0023,
    MyHTML_TAG_DD                                                       = $0024,
    MyHTML_TAG_DEL                                                      = $0025,
    MyHTML_TAG_DETAILS                                                  = $0026,
    MyHTML_TAG_DFN                                                      = $0027,
    MyHTML_TAG_DIALOG                                                   = $0028,
    MyHTML_TAG_DIR                                                      = $0029,
    MyHTML_TAG_DIV                                                      = $002a,
    MyHTML_TAG_DL                                                       = $002b,
    MyHTML_TAG_DT                                                       = $002c,
    MyHTML_TAG_EM                                                       = $002d,
    MyHTML_TAG_EMBED                                                    = $002e,
    MyHTML_TAG_FIELDSET                                                 = $002f,
    MyHTML_TAG_FIGCAPTION                                               = $0030,
    MyHTML_TAG_FIGURE                                                   = $0031,
    MyHTML_TAG_FONT                                                     = $0032,
    MyHTML_TAG_FOOTER                                                   = $0033,
    MyHTML_TAG_FORM                                                     = $0034,
    MyHTML_TAG_FRAME                                                    = $0035,
    MyHTML_TAG_FRAMESET                                                 = $0036,
    MyHTML_TAG_H1                                                       = $0037,
    MyHTML_TAG_H2                                                       = $0038,
    MyHTML_TAG_H3                                                       = $0039,
    MyHTML_TAG_H4                                                       = $003a,
    MyHTML_TAG_H5                                                       = $003b,
    MyHTML_TAG_H6                                                       = $003c,
    MyHTML_TAG_HEAD                                                     = $003d,
    MyHTML_TAG_HEADER                                                   = $003e,
    MyHTML_TAG_HGROUP                                                   = $003f,
    MyHTML_TAG_HR                                                       = $0040,
    MyHTML_TAG_HTML                                                     = $0041,
    MyHTML_TAG_I                                                        = $0042,
    MyHTML_TAG_IFRAME                                                   = $0043,
    MyHTML_TAG_IMAGE                                                    = $0044,
    MyHTML_TAG_IMG                                                      = $0045,
    MyHTML_TAG_INPUT                                                    = $0046,
    MyHTML_TAG_INS                                                      = $0047,
    MyHTML_TAG_ISINDEX                                                  = $0048,
    MyHTML_TAG_KBD                                                      = $0049,
    MyHTML_TAG_KEYGEN                                                   = $004a,
    MyHTML_TAG_LABEL                                                    = $004b,
    MyHTML_TAG_LEGEND                                                   = $004c,
    MyHTML_TAG_LI                                                       = $004d,
    MyHTML_TAG_LINK                                                     = $004e,
    MyHTML_TAG_LISTING                                                  = $004f,
    MyHTML_TAG_MAIN                                                     = $0050,
    MyHTML_TAG_MAP                                                      = $0051,
    MyHTML_TAG_MARK                                                     = $0052,
    MyHTML_TAG_MARQUEE                                                  = $0053,
    MyHTML_TAG_MENU                                                     = $0054,
    MyHTML_TAG_MENUITEM                                                 = $0055,
    MyHTML_TAG_META                                                     = $0056,
    MyHTML_TAG_METER                                                    = $0057,
    MyHTML_TAG_MTEXT                                                    = $0058,
    MyHTML_TAG_NAV                                                      = $0059,
    MyHTML_TAG_NOBR                                                     = $005a,
    MyHTML_TAG_NOEMBED                                                  = $005b,
    MyHTML_TAG_NOFRAMES                                                 = $005c,
    MyHTML_TAG_NOSCRIPT                                                 = $005d,
    MyHTML_TAG_OBJECT                                                   = $005e,
    MyHTML_TAG_OL                                                       = $005f,
    MyHTML_TAG_OPTGROUP                                                 = $0060,
    MyHTML_TAG_OPTION                                                   = $0061,
    MyHTML_TAG_OUTPUT                                                   = $0062,
    MyHTML_TAG_P                                                        = $0063,
    MyHTML_TAG_PARAM                                                    = $0064,
    MyHTML_TAG_PLAINTEXT                                                = $0065,
    MyHTML_TAG_PRE                                                      = $0066,
    MyHTML_TAG_PROGRESS                                                 = $0067,
    MyHTML_TAG_Q                                                        = $0068,
    MyHTML_TAG_RB                                                       = $0069,
    MyHTML_TAG_RP                                                       = $006a,
    MyHTML_TAG_RT                                                       = $006b,
    MyHTML_TAG_RTC                                                      = $006c,
    MyHTML_TAG_RUBY                                                     = $006d,
    MyHTML_TAG_S                                                        = $006e,
    MyHTML_TAG_SAMP                                                     = $006f,
    MyHTML_TAG_SCRIPT                                                   = $0070,
    MyHTML_TAG_SECTION                                                  = $0071,
    MyHTML_TAG_SELECT                                                   = $0072,
    MyHTML_TAG_SMALL                                                    = $0073,
    MyHTML_TAG_SOURCE                                                   = $0074,
    MyHTML_TAG_SPAN                                                     = $0075,
    MyHTML_TAG_STRIKE                                                   = $0076,
    MyHTML_TAG_STRONG                                                   = $0077,
    MyHTML_TAG_STYLE                                                    = $0078,
    MyHTML_TAG_SUB                                                      = $0079,
    MyHTML_TAG_SUMMARY                                                  = $007a,
    MyHTML_TAG_SUP                                                      = $007b,
    MyHTML_TAG_SVG                                                      = $007c,
    MyHTML_TAG_TABLE                                                    = $007d,
    MyHTML_TAG_TBODY                                                    = $007e,
    MyHTML_TAG_TD                                                       = $007f,
    MyHTML_TAG_TEMPLATE                                                 = $0080,
    MyHTML_TAG_TEXTAREA                                                 = $0081,
    MyHTML_TAG_TFOOT                                                    = $0082,
    MyHTML_TAG_TH                                                       = $0083,
    MyHTML_TAG_THEAD                                                    = $0084,
    MyHTML_TAG_TIME                                                     = $0085,
    MyHTML_TAG_TITLE                                                    = $0086,
    MyHTML_TAG_TR                                                       = $0087,
    MyHTML_TAG_TRACK                                                    = $0088,
    MyHTML_TAG_TT                                                       = $0089,
    MyHTML_TAG_U                                                        = $008a,
    MyHTML_TAG_UL                                                       = $008b,
    MyHTML_TAG_VAR                                                      = $008c,
    MyHTML_TAG_VIDEO                                                    = $008d,
    MyHTML_TAG_WBR                                                      = $008e,
    MyHTML_TAG_XMP                                                      = $008f,
    MyHTML_TAG_ALTGLYPH                                                 = $0090,
    MyHTML_TAG_ALTGLYPHDEF                                              = $0091,
    MyHTML_TAG_ALTGLYPHITEM                                             = $0092,
    MyHTML_TAG_ANIMATE                                                  = $0093,
    MyHTML_TAG_ANIMATECOLOR                                             = $0094,
    MyHTML_TAG_ANIMATEMOTION                                            = $0095,
    MyHTML_TAG_ANIMATETRANSFORM                                         = $0096,
    MyHTML_TAG_CIRCLE                                                   = $0097,
    MyHTML_TAG_CLIPPATH                                                 = $0098,
    MyHTML_TAG_COLOR_PROFILE                                            = $0099,
    MyHTML_TAG_CURSOR                                                   = $009a,
    MyHTML_TAG_DEFS                                                     = $009b,
    MyHTML_TAG_DESC                                                     = $009c,
    MyHTML_TAG_ELLIPSE                                                  = $009d,
    MyHTML_TAG_FEBLEND                                                  = $009e,
    MyHTML_TAG_FECOLORMATRIX                                            = $009f,
    MyHTML_TAG_FECOMPONENTTRANSFER                                      = $00a0,
    MyHTML_TAG_FECOMPOSITE                                              = $00a1,
    MyHTML_TAG_FECONVOLVEMATRIX                                         = $00a2,
    MyHTML_TAG_FEDIFFUSELIGHTING                                        = $00a3,
    MyHTML_TAG_FEDISPLACEMENTMAP                                        = $00a4,
    MyHTML_TAG_FEDISTANTLIGHT                                           = $00a5,
    MyHTML_TAG_FEDROPSHADOW                                             = $00a6,
    MyHTML_TAG_FEFLOOD                                                  = $00a7,
    MyHTML_TAG_FEFUNCA                                                  = $00a8,
    MyHTML_TAG_FEFUNCB                                                  = $00a9,
    MyHTML_TAG_FEFUNCG                                                  = $00aa,
    MyHTML_TAG_FEFUNCR                                                  = $00ab,
    MyHTML_TAG_FEGAUSSIANBLUR                                           = $00ac,
    MyHTML_TAG_FEIMAGE                                                  = $00ad,
    MyHTML_TAG_FEMERGE                                                  = $00ae,
    MyHTML_TAG_FEMERGENODE                                              = $00af,
    MyHTML_TAG_FEMORPHOLOGY                                             = $00b0,
    MyHTML_TAG_FEOFFSET                                                 = $00b1,
    MyHTML_TAG_FEPOINTLIGHT                                             = $00b2,
    MyHTML_TAG_FESPECULARLIGHTING                                       = $00b3,
    MyHTML_TAG_FESPOTLIGHT                                              = $00b4,
    MyHTML_TAG_FETILE                                                   = $00b5,
    MyHTML_TAG_FETURBULENCE                                             = $00b6,
    MyHTML_TAG_FILTER                                                   = $00b7,
    MyHTML_TAG_FONT_FACE                                                = $00b8,
    MyHTML_TAG_FONT_FACE_FORMAT                                         = $00b9,
    MyHTML_TAG_FONT_FACE_NAME                                           = $00ba,
    MyHTML_TAG_FONT_FACE_SRC                                            = $00bb,
    MyHTML_TAG_FONT_FACE_URI                                            = $00bc,
    MyHTML_TAG_FOREIGNOBJECT                                            = $00bd,
    MyHTML_TAG_G                                                        = $00be,
    MyHTML_TAG_GLYPH                                                    = $00bf,
    MyHTML_TAG_GLYPHREF                                                 = $00c0,
    MyHTML_TAG_HKERN                                                    = $00c1,
    MyHTML_TAG_LINE                                                     = $00c2,
    MyHTML_TAG_LINEARGRADIENT                                           = $00c3,
    MyHTML_TAG_MARKER                                                   = $00c4,
    MyHTML_TAG_MASK                                                     = $00c5,
    MyHTML_TAG_METADATA                                                 = $00c6,
    MyHTML_TAG_MISSING_GLYPH                                            = $00c7,
    MyHTML_TAG_MPATH                                                    = $00c8,
    MyHTML_TAG_PATH                                                     = $00c9,
    MyHTML_TAG_PATTERN                                                  = $00ca,
    MyHTML_TAG_POLYGON                                                  = $00cb,
    MyHTML_TAG_POLYLINE                                                 = $00cc,
    MyHTML_TAG_RADIALGRADIENT                                           = $00cd,
    MyHTML_TAG_RECT                                                     = $00ce,
    MyHTML_TAG_SET                                                      = $00cf,
    MyHTML_TAG_STOP                                                     = $00d0,
    MyHTML_TAG_SWITCH                                                   = $00d1,
    MyHTML_TAG_SYMBOL                                                   = $00d2,
    MyHTML_TAG_TEXT                                                     = $00d3,
    MyHTML_TAG_TEXTPATH                                                 = $00d4,
    MyHTML_TAG_TREF                                                     = $00d5,
    MyHTML_TAG_TSPAN                                                    = $00d6,
    MyHTML_TAG_USE                                                      = $00d7,
    MyHTML_TAG_VIEW                                                     = $00d8,
    MyHTML_TAG_VKERN                                                    = $00d9,
    MyHTML_TAG_MATH                                                     = $00da,
    MyHTML_TAG_MACTION                                                  = $00db,
    MyHTML_TAG_MALIGNGROUP                                              = $00dc,
    MyHTML_TAG_MALIGNMARK                                               = $00dd,
    MyHTML_TAG_MENCLOSE                                                 = $00de,
    MyHTML_TAG_MERROR                                                   = $00df,
    MyHTML_TAG_MFENCED                                                  = $00e0,
    MyHTML_TAG_MFRAC                                                    = $00e1,
    MyHTML_TAG_MGLYPH                                                   = $00e2,
    MyHTML_TAG_MI                                                       = $00e3,
    MyHTML_TAG_MLABELEDTR                                               = $00e4,
    MyHTML_TAG_MLONGDIV                                                 = $00e5,
    MyHTML_TAG_MMULTISCRIPTS                                            = $00e6,
    MyHTML_TAG_MN                                                       = $00e7,
    MyHTML_TAG_MO                                                       = $00e8,
    MyHTML_TAG_MOVER                                                    = $00e9,
    MyHTML_TAG_MPADDED                                                  = $00ea,
    MyHTML_TAG_MPHANTOM                                                 = $00eb,
    MyHTML_TAG_MROOT                                                    = $00ec,
    MyHTML_TAG_MROW                                                     = $00ed,
    MyHTML_TAG_MS                                                       = $00ee,
    MyHTML_TAG_MSCARRIES                                                = $00ef,
    MyHTML_TAG_MSCARRY                                                  = $00f0,
    MyHTML_TAG_MSGROUP                                                  = $00f1,
    MyHTML_TAG_MSLINE                                                   = $00f2,
    MyHTML_TAG_MSPACE                                                   = $00f3,
    MyHTML_TAG_MSQRT                                                    = $00f4,
    MyHTML_TAG_MSROW                                                    = $00f5,
    MyHTML_TAG_MSTACK                                                   = $00f6,
    MyHTML_TAG_MSTYLE                                                   = $00f7,
    MyHTML_TAG_MSUB                                                     = $00f8,
    MyHTML_TAG_MSUP                                                     = $00f9,
    MyHTML_TAG_MSUBSUP                                                  = $00fa,
    MyHTML_TAG__END_OF_FILE                                             = $00fb,

    MyHTML_TAG_FIRST_ENTRY                          = Longint(MyHTML_TAG__TEXT){%H-},
    MyHTML_TAG_LAST_ENTRY                                               = $00fc
  );

  myhtml_callback_token_f = function (tree : pmyhtml_tree_t; token :
    pmyhtml_token_node_t; ctx : Pointer) : Pointer of object;
  myhtml_callback_tree_node_f = procedure (tree : pmyhtml_tree_t; node :
    pmyhtml_tree_node_t; ctx : Pointer) of object;

(*myhtml/myosi.h***************************************************************)

type
  (* tree *)
  myhtml_tree_flags = (
    MyHTML_TREE_FLAGS_CLEAN                                             = $0000,
    MyHTML_TREE_FLAGS_SCRIPT                                            = $0001,
    MyHTML_TREE_FLAGS_FRAMESET_OK                                       = $0002,
    MyHTML_TREE_FLAGS_IFRAME_SRCDOC                                     = $0004,
    MyHTML_TREE_FLAGS_ALREADY_STARTED                                   = $0008,
    MyHTML_TREE_FLAGS_SINGLE_MODE                                       = $0010,
    MyHTML_TREE_FLAGS_PARSE_END                                         = $0020,
    MyHTML_TREE_FLAGS_PARSE_FLAG                                        = $0040,
    MyHTML_TREE_FLAGS_PARSE_FLAG_EMIT_NEWLINE                           = $0080
  );

  pmyhtml_tree_parse_flags_t = ^myhtml_tree_parse_flags_t;
  myhtml_tree_parse_flags_t = (
    MyHTML_TREE_PARSE_FLAGS_CLEAN                                       = $0000,
    MyHTML_TREE_PARSE_FLAGS_WITHOUT_BUILD_TREE                          = $0001,
    MyHTML_TREE_PARSE_FLAGS_WITHOUT_PROCESS_TOKEN                       = $0003,
    (* skip ws token, but not for RCDATA, RAWTEXT, CDATA and PLAINTEXT *)
    MyHTML_TREE_PARSE_FLAGS_SKIP_WHITESPACE_TOKEN                       = $0004,
    MyHTML_TREE_PARSE_FLAGS_WITHOUT_DOCTYPE_IN_TREE                     = $0008
  );

  pmyhtml_token_type_t = ^myhtml_token_type_t;
  myhtml_token_type_t = (
    MyHTML_TOKEN_TYPE_OPEN                                              = $0000,
    MyHTML_TOKEN_TYPE_CLOSE                                             = $0001,
    MyHTML_TOKEN_TYPE_CLOSE_SELF                                        = $0002,
    MyHTML_TOKEN_TYPE_DONE                                              = $0004,
    MyHTML_TOKEN_TYPE_WHITESPACE                                        = $0008,
    MyHTML_TOKEN_TYPE_RCDATA                                            = $0010,
    MyHTML_TOKEN_TYPE_RAWTEXT                                           = $0020,
    MyHTML_TOKEN_TYPE_SCRIPT                                            = $0040,
    MyHTML_TOKEN_TYPE_PLAINTEXT                                         = $0080,
    MyHTML_TOKEN_TYPE_CDATA                                             = $0100,
    MyHTML_TOKEN_TYPE_DATA                                              = $0200,
    MyHTML_TOKEN_TYPE_COMMENT                                           = $0400,
    MyHTML_TOKEN_TYPE_NULL                                              = $0800
  );

  pmyhtml_token_index_t = ^myhtml_token_index_t;
  myhtml_token_index_t = type QWord;

  pmyhtml_token_attr_index_t = ^myhtml_token_attr_index_t;
  myhtml_token_attr_index_t = type QWord;

  (* tags *)
  pmyhtml_tag_categories_t = ^myhtml_tag_categories_t;
  myhtml_tag_categories_t = (
    MyHTML_TAG_CATEGORIES_UNDEF                                         = $0000,
    MyHTML_TAG_CATEGORIES_ORDINARY                                      = $0001,
    MyHTML_TAG_CATEGORIES_SPECIAL                                       = $0002,
    MyHTML_TAG_CATEGORIES_FORMATTING                                    = $0004,
    MyHTML_TAG_CATEGORIES_SCOPE                                         = $0008,
    MyHTML_TAG_CATEGORIES_SCOPE_LIST_ITEM                               = $0010,
    MyHTML_TAG_CATEGORIES_SCOPE_BUTTON                                  = $0020,
    MyHTML_TAG_CATEGORIES_SCOPE_TABLE                                   = $0040,
    MyHTML_TAG_CATEGORIES_SCOPE_SELECT                                  = $0080
  );

  (* parse *)
  pmyhtml_tokenizer_state_t = ^myhtml_tokenizer_state_t;
  myhtml_tokenizer_state_t = (
    MyHTML_TOKENIZER_STATE_DATA                                         = $0000,
    MyHTML_TOKENIZER_STATE_CHARACTER_REFERENCE_IN_DATA                  = $0001,
    MyHTML_TOKENIZER_STATE_RCDATA                                       = $0002,
    MyHTML_TOKENIZER_STATE_CHARACTER_REFERENCE_IN_RCDATA                = $0003,
    MyHTML_TOKENIZER_STATE_RAWTEXT                                      = $0004,
    MyHTML_TOKENIZER_STATE_SCRIPT_DATA                                  = $0005,
    MyHTML_TOKENIZER_STATE_PLAINTEXT                                    = $0006,
    MyHTML_TOKENIZER_STATE_TAG_OPEN                                     = $0007,
    MyHTML_TOKENIZER_STATE_END_TAG_OPEN                                 = $0008,
    MyHTML_TOKENIZER_STATE_TAG_NAME                                     = $0009,
    MyHTML_TOKENIZER_STATE_RCDATA_LESS_THAN_SIGN                        = $000a,
    MyHTML_TOKENIZER_STATE_RCDATA_END_TAG_OPEN                          = $000b,
    MyHTML_TOKENIZER_STATE_RCDATA_END_TAG_NAME                          = $000c,
    MyHTML_TOKENIZER_STATE_RAWTEXT_LESS_THAN_SIGN                       = $000d,
    MyHTML_TOKENIZER_STATE_RAWTEXT_END_TAG_OPEN                         = $000e,
    MyHTML_TOKENIZER_STATE_RAWTEXT_END_TAG_NAME                         = $000f,
    MyHTML_TOKENIZER_STATE_SCRIPT_DATA_LESS_THAN_SIGN                   = $0010,
    MyHTML_TOKENIZER_STATE_SCRIPT_DATA_END_TAG_OPEN                     = $0011,
    MyHTML_TOKENIZER_STATE_SCRIPT_DATA_END_TAG_NAME                     = $0012,
    MyHTML_TOKENIZER_STATE_SCRIPT_DATA_ESCAPE_START                     = $0013,
    MyHTML_TOKENIZER_STATE_SCRIPT_DATA_ESCAPE_START_DASH                = $0014,
    MyHTML_TOKENIZER_STATE_SCRIPT_DATA_ESCAPED                          = $0015,
    MyHTML_TOKENIZER_STATE_SCRIPT_DATA_ESCAPED_DASH                     = $0016,
    MyHTML_TOKENIZER_STATE_SCRIPT_DATA_ESCAPED_DASH_DASH                = $0017,
    MyHTML_TOKENIZER_STATE_SCRIPT_DATA_ESCAPED_LESS_THAN_SIGN           = $0018,
    MyHTML_TOKENIZER_STATE_SCRIPT_DATA_ESCAPED_END_TAG_OPEN             = $0019,
    MyHTML_TOKENIZER_STATE_SCRIPT_DATA_ESCAPED_END_TAG_NAME             = $001a,
    MyHTML_TOKENIZER_STATE_SCRIPT_DATA_DOUBLE_ESCAPE_START              = $001b,
    MyHTML_TOKENIZER_STATE_SCRIPT_DATA_DOUBLE_ESCAPED                   = $001c,
    MyHTML_TOKENIZER_STATE_SCRIPT_DATA_DOUBLE_ESCAPED_DASH              = $001d,
    MyHTML_TOKENIZER_STATE_SCRIPT_DATA_DOUBLE_ESCAPED_DASH_DASH         = $001e,
    MyHTML_TOKENIZER_STATE_SCRIPT_DATA_DOUBLE_ESCAPED_LESS_THAN_SIGN    = $001f,
    MyHTML_TOKENIZER_STATE_SCRIPT_DATA_DOUBLE_ESCAPE_END                = $0020,
    MyHTML_TOKENIZER_STATE_BEFORE_ATTRIBUTE_NAME                        = $0021,
    MyHTML_TOKENIZER_STATE_ATTRIBUTE_NAME                               = $0022,
    MyHTML_TOKENIZER_STATE_AFTER_ATTRIBUTE_NAME                         = $0023,
    MyHTML_TOKENIZER_STATE_BEFORE_ATTRIBUTE_VALUE                       = $0024,
    MyHTML_TOKENIZER_STATE_ATTRIBUTE_VALUE_DOUBLE_QUOTED                = $0025,
    MyHTML_TOKENIZER_STATE_ATTRIBUTE_VALUE_SINGLE_QUOTED                = $0026,
    MyHTML_TOKENIZER_STATE_ATTRIBUTE_VALUE_UNQUOTED                     = $0027,
    MyHTML_TOKENIZER_STATE_CHARACTER_REFERENCE_IN_ATTRIBUTE_VALUE       = $0028,
    MyHTML_TOKENIZER_STATE_AFTER_ATTRIBUTE_VALUE_QUOTED                 = $0029,
    MyHTML_TOKENIZER_STATE_SELF_CLOSING_START_TAG                       = $002a,
    MyHTML_TOKENIZER_STATE_BOGUS_COMMENT                                = $002b,
    MyHTML_TOKENIZER_STATE_MARKUP_DECLARATION_OPEN                      = $002c,
    MyHTML_TOKENIZER_STATE_COMMENT_START                                = $002d,
    MyHTML_TOKENIZER_STATE_COMMENT_START_DASH                           = $002e,
    MyHTML_TOKENIZER_STATE_COMMENT                                      = $002f,
    MyHTML_TOKENIZER_STATE_COMMENT_END_DASH                             = $0030,
    MyHTML_TOKENIZER_STATE_COMMENT_END                                  = $0031,
    MyHTML_TOKENIZER_STATE_COMMENT_END_BANG                             = $0032,
    MyHTML_TOKENIZER_STATE_DOCTYPE                                      = $0033,
    MyHTML_TOKENIZER_STATE_BEFORE_DOCTYPE_NAME                          = $0034,
    MyHTML_TOKENIZER_STATE_DOCTYPE_NAME                                 = $0035,
    MyHTML_TOKENIZER_STATE_AFTER_DOCTYPE_NAME                           = $0036,
    MyHTML_TOKENIZER_STATE_AFTER_DOCTYPE_PUBLIC_KEYWORD                 = $0037,
    MyHTML_TOKENIZER_STATE_BEFORE_DOCTYPE_PUBLIC_IDENTIFIER             = $0038,
    MyHTML_TOKENIZER_STATE_DOCTYPE_PUBLIC_IDENTIFIER_DOUBLE_QUOTED      = $0039,
    MyHTML_TOKENIZER_STATE_DOCTYPE_PUBLIC_IDENTIFIER_SINGLE_QUOTED      = $003a,
    MyHTML_TOKENIZER_STATE_AFTER_DOCTYPE_PUBLIC_IDENTIFIER              = $003b,
    MyHTML_TOKENIZER_STATE_BETWEEN_DOCTYPE_PUBLIC_AND_SYSTEM_IDENTIFIERS= $003c,
    MyHTML_TOKENIZER_STATE_AFTER_DOCTYPE_SYSTEM_KEYWORD                 = $003d,
    MyHTML_TOKENIZER_STATE_BEFORE_DOCTYPE_SYSTEM_IDENTIFIER             = $003e,
    MyHTML_TOKENIZER_STATE_DOCTYPE_SYSTEM_IDENTIFIER_DOUBLE_QUOTED      = $003f,
    MyHTML_TOKENIZER_STATE_DOCTYPE_SYSTEM_IDENTIFIER_SINGLE_QUOTED      = $0040,
    MyHTML_TOKENIZER_STATE_AFTER_DOCTYPE_SYSTEM_IDENTIFIER              = $0041,
    MyHTML_TOKENIZER_STATE_BOGUS_DOCTYPE                                = $0042,
    MyHTML_TOKENIZER_STATE_CDATA_SECTION                                = $0043,
    MyHTML_TOKENIZER_STATE_CUSTOM_AFTER_DOCTYPE_NAME_A_Z                = $0044,
    MyHTML_TOKENIZER_STATE_PARSE_ERROR_STOP                             = $0045,

    MyHTML_TOKENIZER_STATE_FIRST_ENTRY = Longint(MyHTML_TOKENIZER_STATE_DATA){%H-},
    MyHTML_TOKENIZER_STATE_LAST_ENTRY                                   = $0046
  );

  pmyhtml_insertion_mode_t = ^myhtml_insertion_mode_t;
  myhtml_insertion_mode_t = (
    MyHTML_INSERTION_MODE_INITIAL                                       = $0000,
    MyHTML_INSERTION_MODE_BEFORE_HTML                                   = $0001,
    MyHTML_INSERTION_MODE_BEFORE_HEAD                                   = $0002,
    MyHTML_INSERTION_MODE_IN_HEAD                                       = $0003,
    MyHTML_INSERTION_MODE_IN_HEAD_NOSCRIPT                              = $0004,
    MyHTML_INSERTION_MODE_AFTER_HEAD                                    = $0005,
    MyHTML_INSERTION_MODE_IN_BODY                                       = $0006,
    MyHTML_INSERTION_MODE_TEXT                                          = $0007,
    MyHTML_INSERTION_MODE_IN_TABLE                                      = $0008,
    MyHTML_INSERTION_MODE_IN_TABLE_TEXT                                 = $0009,
    MyHTML_INSERTION_MODE_IN_CAPTION                                    = $000a,
    MyHTML_INSERTION_MODE_IN_COLUMN_GROUP                               = $000b,
    MyHTML_INSERTION_MODE_IN_TABLE_BODY                                 = $000c,
    MyHTML_INSERTION_MODE_IN_ROW                                        = $000d,
    MyHTML_INSERTION_MODE_IN_CELL                                       = $000e,
    MyHTML_INSERTION_MODE_IN_SELECT                                     = $000f,
    MyHTML_INSERTION_MODE_IN_SELECT_IN_TABLE                            = $0010,
    MyHTML_INSERTION_MODE_IN_TEMPLATE                                   = $0011,
    MyHTML_INSERTION_MODE_AFTER_BODY                                    = $0012,
    MyHTML_INSERTION_MODE_IN_FRAMESET                                   = $0013,
    MyHTML_INSERTION_MODE_AFTER_FRAMESET                                = $0014,
    MyHTML_INSERTION_MODE_AFTER_AFTER_BODY                              = $0015,
    MyHTML_INSERTION_MODE_AFTER_AFTER_FRAMESET                          = $0016,

    MyHTML_INSERTION_MODE_LAST_ENTRY                                    = $0017
  );

  (* base *)
  pmyhtml_status_t = ^myhtml_status_t;
  myhtml_status_t = (
    MyHTML_STATUS_OK                                                    = $0000,
    MyHTML_STATUS_ERROR                                                 = $0001,
    MyHTML_STATUS_ERROR_MEMORY_ALLOCATION                               = $0002,
    MyHTML_STATUS_RULES_ERROR_MEMORY_ALLOCATION                         = $9064,
    MyHTML_STATUS_TOKENIZER_ERROR_MEMORY_ALLOCATION                     = $912c,
    MyHTML_STATUS_TOKENIZER_ERROR_FRAGMENT_INIT                         = $912d,
    MyHTML_STATUS_TAGS_ERROR_MEMORY_ALLOCATION                          = $9190,
    MyHTML_STATUS_TAGS_ERROR_MCOBJECT_CREATE                            = $9191,
    MyHTML_STATUS_TAGS_ERROR_MCOBJECT_MALLOC                            = $9192,
    MyHTML_STATUS_TAGS_ERROR_MCOBJECT_CREATE_NODE                       = $9193,
    MyHTML_STATUS_TAGS_ERROR_CACHE_MEMORY_ALLOCATION                    = $9194,
    MyHTML_STATUS_TAGS_ERROR_INDEX_MEMORY_ALLOCATION                    = $9195,
    MyHTML_STATUS_TREE_ERROR_MEMORY_ALLOCATION                          = $91f4,
    MyHTML_STATUS_TREE_ERROR_MCOBJECT_CREATE                            = $91f5,
    MyHTML_STATUS_TREE_ERROR_MCOBJECT_INIT                              = $91f6,
    MyHTML_STATUS_TREE_ERROR_MCOBJECT_CREATE_NODE                       = $91f7,
    MyHTML_STATUS_TREE_ERROR_INCOMING_BUFFER_CREATE                     = $91f8,
    MyHTML_STATUS_ATTR_ERROR_ALLOCATION                                 = $9258,
    MyHTML_STATUS_ATTR_ERROR_CREATE                                     = $9259,
    MyHTML_STATUS_STREAM_BUFFER_ERROR_CREATE                            = $9300,
    MyHTML_STATUS_STREAM_BUFFER_ERROR_INIT                              = $9301,
    MyHTML_STATUS_STREAM_BUFFER_ENTRY_ERROR_CREATE                      = $9302,
    MyHTML_STATUS_STREAM_BUFFER_ENTRY_ERROR_INIT                        = $9303,
    MyHTML_STATUS_STREAM_BUFFER_ERROR_ADD_ENTRY                         = $9304
  );

  pmyhtml_namespace_t = ^myhtml_namespace_t;
  myhtml_namespace_t = (
    MyHTML_NAMESPACE_UNDEF                                              = $0000,
    MyHTML_NAMESPACE_HTML                                               = $0001,
    MyHTML_NAMESPACE_MATHML                                             = $0002,
    MyHTML_NAMESPACE_SVG                                                = $0003,
    MyHTML_NAMESPACE_XLINK                                              = $0004,
    MyHTML_NAMESPACE_XML                                                = $0005,
    MyHTML_NAMESPACE_XMLNS                                              = $0006,
    MyHTML_NAMESPACE_ANY                                                = $0007,

    MyHTML_NAMESPACE_LAST_ENTRY                                         = $0007
  {%H-});

  pmyhtml_options_t = ^myhtml_options_t;
  myhtml_options_t = (
    MyHTML_OPTIONS_DEFAULT                                              = $0000,
    MyHTML_OPTIONS_PARSE_MODE_SINGLE                                    = $0001,
    MyHTML_OPTIONS_PARSE_MODE_ALL_IN_ONE                                = $0002,
    MyHTML_OPTIONS_PARSE_MODE_SEPARATELY                                = $0004
  );

  pmyhtml_position_t = ^myhtml_position_t;
  myhtml_position_t = record
    start : QWord;
    length : QWord;
  end;

  pmyhtml_version_t = ^myhtml_version_t;
  myhtml_version_t = record
    major : Integer;
    minor : Integer;
    patch : Integer;
  end;

(*myhtml/token.h***************************************************************)

type
  pmyhtml_token_replacement_entry_t = ^myhtml_token_replacement_entry_t;
  myhtml_token_replacement_entry_t = record
    from : PChar;
    from_size : QWord;

    to_str : PChar;
    to_size : QWord;
  end;

  pmyhtml_token_namespace_replacement_t = ^myhtml_token_namespace_replacement_t;
  myhtml_token_namespace_replacement_t = record
    from : PChar;
    from_size : QWord;

    to_str : PChar;
    to_size : QWord;

    ns : myhtml_namespace_t;
  end;

  pmyhtml_token_attr_t = ^myhtml_token_attr_t;
  myhtml_token_attr_t = record
    next : pmyhtml_token_attr_t;
    prev : pmyhtml_token_attr_t;

    key : mycore_string_t;
    value : mycore_string_t;

    raw_key_begin : QWord;
    raw_key_length : QWord;
    raw_value_begin : QWord;
    raw_value_length : QWord;

    ns : myhtml_namespace_t;
  end;

  myhtml_token_node_t = record
    tag_id : myhtml_tag_id_t;

    str : mycore_string_t;

    raw_begin : QWord;
    raw_length : QWord;

    element_begin : QWord;
    element_length : QWord;

    attr_first : pmyhtml_token_attr_t;
    attr_last : pmyhtml_token_attr_t;

    token_type : pmyhtml_token_type_t;
  end;

(*myhtml/api.h*****************************************************************)

type
  pmyhtml_tree_attr_t = ^myhtml_tree_attr_t;
  myhtml_tree_attr_t = type myhtml_token_attr_t;

(*myhtml/stream.h**************************************************************)

type
  pmyhtml_stream_buffer_entry_t = ^myhtml_stream_buffer_entry_t;
  myhtml_stream_buffer_t = record
    data : PChar;
    length : QWord;
    size : QWord;
  end;

  pmyhtml_stream_buffer_t = ^myhtml_stream_buffer_t;
  myhtml_stream_buffer_t = record
    entries : pmyhtml_stream_buffer_entry_t;
    length : QWord;
    size : QWord;

    res : myencoding_result_t;
  end;

function myhtml_stream_buffer_create : pmyhtml_stream_buffer_t; cdecl;
  external MyHTMLLib;
function myhtml_stream_buffer_init (stream_buffer : pmyhtml_stream_buffer_t;
  entries_size : QWord) : mystatus_t; cdecl; external MyHTMLLib;
procedure myhtml_stream_buffer_clean (stream_buffer : pmyhtml_stream_buffer_t);
  cdecl; external MyHTMLLib;
function myhtml_stream_buffer_destroy (stream_buffer : pmyhtml_stream_buffer_t;
  self_destroy : Boolean) : pmyhtml_stream_buffer_t; cdecl; external MyHTMLLib;
function myhtml_stream_buffer_add_entry (stream_buffer :
  pmyhtml_stream_buffer_t; entry_data_size : QWord) :
  pmyhtml_stream_buffer_entry_t; cdecl; external MyHTMLLib;
function myhtml_stream_buffer_current_entry (stream_buffer :
  pmyhtml_stream_buffer_t) : pmyhtml_stream_buffer_entry_t; cdecl;
  external MyHTMLLib;
function myhtml_stream_buffer_entry_init (stream_buffer_entry :
  pmyhtml_stream_buffer_entry_t; size : QWord) : mystatus_t; cdecl;
  external MyHTMLLib;
procedure myhtml_stream_buffer_entry_clean (stream_buffer_entry :
  pmyhtml_stream_buffer_entry_t); cdecl; external MyHTMLLib;
function myhtml_stream_buffer_entry_destroy (stream_buffer_entry :
  pmyhtml_stream_buffer_entry_t; self_destroy : Boolean) :
  pmyhtml_stream_buffer_entry_t; cdecl; external MyHTMLLib;

(*myhtml/tree.h****************************************************************)

type
  pmyhtml_tree_node_type = ^myhtml_tree_node_type;
  myhtml_tree_node_type = (
    MyHTML_TYPE_NONE                                                    = 0,
    MyHTML_TYPE_BLOCK                                                   = 1,
    MyHTML_TYPE_INLINE                                                  = 2,
    MyHTML_TYPE_TABLE                                                   = 3,
    MyHTML_TYPE_META                                                    = 4,
    MyHTML_TYPE_COMMENT                                                 = 5
  );

  pmyhtml_close_type_t = ^myhtml_close_type_t;
  myhtml_close_type_t = (
    MyHTML_CLOSE_TYPE_NONE                                              = 0,
    MyHTML_CLOSE_TYPE_NOW                                               = 1,
    MyHTML_CLOSE_TYPE_SELF                                              = 2,
    MyHTML_CLOSE_TYPE_BLOCK                                             = 3
  );

  pmyhtml_tree_node_flags_t = ^myhtml_tree_node_flags_t;
  myhtml_tree_node_flags_t = (
    MyHTML_TREE_NODE_UNDEF                                              = 0,
    MyHTML_TREE_NODE_PARSER_INSERTED                                    = 1,
    MyHTML_TREE_NODE_BLOCKING                                           = 2
  );

  ppmyhtml_tree_node_t = ^pmyhtml_tree_node_t;
  myhtml_tree_node_t = record
    flags : myhtml_tree_node_flags_t;

    tag_id : myhtml_tag_id_t;
    ns : myhtml_namespace_t;

    prev : pmyhtml_tree_node_t;
    next : pmyhtml_tree_node_t;
    child : pmyhtml_tree_node_t;
    parent : pmyhtml_tree_node_t;

    last_child : pmyhtml_tree_node_t;

    token : pmyhtml_token_node_t;
    data : Pointer;

    tree : pmyhtml_tree_t;
  end;

  pmyhtml_tree_compat_mode_t = ^myhtml_tree_compat_mode_t;
  myhtml_tree_compat_mode_t = (
    MyHTML_TREE_COMPAT_MODE_NO_QUIRKS                                   = $0000,
    MyHTML_TREE_COMPAT_MODE_QUIRKS                                      = $0001,
    MyHTML_TREE_COMPAT_MODE_LIMITED_QUIRKS                              = $0002
  );

  pmyhtml_tree_doctype_id_t = ^myhtml_tree_doctype_id_t;
  myhtml_tree_doctype_id_t = (
    MyHTML_TREE_DOCTYPE_ID_NAME                                         = $0000,
    MyHTML_TREE_DOCTYPE_ID_SYSTEM                                       = $0001,
    MyHTML_TREE_DOCTYPE_ID_PUBLIC                                       = $0002
  );

  pmyhtml_tree_insertion_mode_t = ^myhtml_tree_insertion_mode_t;
  myhtml_tree_insertion_mode_t = (
    MyHTML_TREE_INSERTION_MODE_DEFAULT                                  = $0000,
    MyHTML_TREE_INSERTION_MODE_BEFORE                                   = $0001,
    MyHTML_TREE_INSERTION_MODE_AFTER                                    = $0002
  );

  pmyhtml_async_args_t = ^myhtml_async_args_t;
  myhtml_async_args_t = record
    mchar_node_id : QWord;
  end;

  pmyhtml_tree_doctype_t = ^myhtml_tree_doctype_t;
  myhtml_tree_doctype_t = record
    is_html : Boolean;
    attr_name : PChar;
    attr_public : PChar;
    attr_system : PChar;
  end;

  pmyhtml_tree_list_t = ^myhtml_tree_list_t;
  myhtml_tree_list_t = record
    list : ppmyhtml_tree_node_t;
    length : QWord;
    size : QWord;
  end;

  pmyhtml_tree_token_list_t = ^myhtml_tree_token_list_t;
  myhtml_tree_token_list_t = record
    list : ppmyhtml_token_node_t;
    length : QWord;
    size : QWord;
  end;

  pmyhtml_tree_insertion_list_t = ^myhtml_tree_insertion_list_t;
  myhtml_tree_insertion_list_t = record
    list : pmyhtml_insertion_mode_t;
    length : QWord;
    size : QWord;
  end;

  ppmyhtml_tree_temp_tag_name_t = ^pmyhtml_tree_temp_tag_name_t;
  pmyhtml_tree_temp_tag_name_t = ^myhtml_tree_temp_tag_name_t;
  myhtml_tree_tag_name = record
    data : PChar;
    length : QWord;
    size : QWord;
  end;

  pmyhtml_tree_special_token_t = ^myhtml_tree_special_token_t;
  myhtml_tree_special_token_t = record
    token : pmyhtml_token_node_t;
    ns : myhtml_namespace_t;
  end;

  pmyhtml_tree_special_token_list_t = ^myhtml_tree_special_token_list_t;
  myhtml_tree_special_token_list_t = record
    list : pmyhtml_tree_special_token_t;
    length : QWord;
    size : QWord;
  end;

  pmyhtml_tree_temp_stream_t = ^myhtml_tree_stream_t;
  myhtml_tree_stream_t = record
    data : ppmyhtml_tree_temp_tag_name_t;
    length : QWord;
    size : QWord;

    res : myencoding_result_t;
    current : pmyhtml_tree_temp_tag_name_t;
  end;

  pmyhtml_tree_t = ^myhtml_tree_t;
  myhtml_tree_t = record
    (* ref *)
    myhtml : pmyhtml_t;
    mchar : pmchar_async_t;
    token : pmyhtml_token_t;
    tree_obj : pmcobject_async_t;
    sync : pmcsync_t;
    queue_entry : pmythread_queue_list_entry_t;
    queue : pmythread_queue_t;
    tags : pmyhtml_tag_t;
    modest : Pointer;
    context : Pointer;

    (* init id's *)
    mcasync_rules_token_id : QWord;
    mcasync_rules_attr_id : QWord;
    mcasync_tree_id : QWord;

    (* mchar_node_id *)
    (* for rules, or if single mode. *)
    (* or for main thread only after parsing *)
    mchar_node_id : QWord;
    attr_current : pmyhtml_token_attr_t;
    tmp_tag_id : myhtml_tag_id_t;
    current_token_node : pmyhtml_token_node_t;
    current_qnode : pmythread_queue_node_t;

    mcobject_incoming_buf : pmcobject_t;
    incoming_buf : pmycore_incoming_buffer_t;
    incoming_buf_first : pmycore_incoming_buffer_t;

    (* ref for nodes *)
    document : pmyhtml_tree_node_t;
    fragment : pmyhtml_tree_node_t;
    node_head : pmyhtml_tree_node_t;
    node_html : pmyhtml_tree_node_t;
    node_body : pmyhtml_tree_node_t;
    node_form : pmyhtml_tree_node_t;
    doctype : myhtml_tree_doctype_t;

    (* for build tree *)
    active_formatting : pmyhtml_tree_list_t;
    open_elements : pmyhtml_tree_list_t;
    other_elements : pmyhtml_tree_list_t;
    token_list : pmyhtml_tree_token_list_t;
    template_insertion : pmyhtml_tree_insertion_list_t;
    async_args : pmyhtml_async_args_t;
    stream_buffer : pmyhtml_stream_buffer_t;
    token_last_done : pmyhtml_token_node_t;

    (* for detect namespace out of tree builder *)
    token_namespace : pmyhtml_token_node_t;

    (* tree params *)
    state : myhtml_tokenizer_state_t;
    state_of_builder : myhtml_tokenizer_state_t;
    insert_mode : myhtml_insertion_mode_t;
    orig_insert_mode : myhtml_insertion_mode_t;
    compat_mode : myhtml_tree_compat_mode_t;
    flags : myhtml_tree_flags;
    parse_flags : myhtml_tree_parse_flags_t;
    foster_parenting : Boolean;
    global_offset : QWord;
    tokenizer_status : mystatus_t;

    encoding : myencoding_t;
    encoding_usereq : myencoding_t;
    temp_tag_name : pmyhtml_tree_temp_tag_name_t;

    (* callback *)
    callback_before_token : myhtml_callback_token_f;
    callback_after_token : myhtml_callback_token_f;

    callback_before_token_ctx : Pointer;
    callback_after_token_ctx : Pointer;

    callback_tree_node_insert : myhtml_callback_tree_node_f;
    callback_tree_node_remove : myhtml_callback_tree_node_f;

    callback_tree_node_insert_ctx : Pointer;
    callback_tree_node_remove_ctx : Pointer;
  end;

(* base *)
function myhtml_tree_create : pmyhtml_tree_t; cdecl; external MyHTMLLib;
function myhtml_tree_init (tree : pmyhtml_tree_t; myhtml : pmyhtml_t) :
  mystatus_t; cdecl; external MyHTMLLib;
procedure myhtml_tree_clean (tree : pmyhtml_tree_t); cdecl; external MyHTMLLib;
procedure myhtml_tree_clean_all (tree : pmyhtml_tree_t); cdecl;
  external MyHTMLLib;
function myhtml_tree_destroy (tree : myhtml_tree_t) : pmyhtml_tree_t; cdecl;
  external MyHTMLLib;

(* parse flags *)
function myhtml_tree_parse_flags (tree : pmyhtml_tree_t) :
  myhtml_tree_parse_flags_t; cdecl; external MyHTMLLib;
procedure myhtml_tree_parse_flags_set (tree : pmyhtml_tree_t; flags :
  myhtml_tree_parse_flags_t); cdecl; external MyHTMLLib;

function myhtml_tree_get_html (tree : pmyhtml_tree_t) : pmyhtml_t; cdecl;
  external MyHTMLLib;
function myhtml_tree_get_tag (tree : pmyhtml_tree_t) : pmyhtml_tag_t; cdecl;
  external MyHTMLLib;
function myhtml_tree_get_document (tree : pmyhtml_tree_t) : pmyhtml_tree_node_t;
  cdecl; external MyHTMLLib;
function myhtml_tree_get_node_html (tree : pmyhtml_tree_t) :
  pmyhtml_tree_node_t; cdecl; external MyHTMLLib;
function myhtml_tree_get_node_head (tree : pmyhtml_tree_t) :
  pmyhtml_tree_node_t; cdecl; external MyHTMLLib;
function myhtml_tree_get_node_body (tree : pmyhtml_tree_t) :
  pmyhtml_tree_node_t; cdecl; external MyHTMLLib;

function myhtml_tree_get_mchar (tree : pmyhtml_tree_t) : pmchar_async_t; cdecl;
  external MyHTMLLib;
function myhtml_tree_get_mchar_node_id (tree : pmyhtml_tree_t) : QWord; cdecl;
  external MyHTMLLib;

(* list *)
function myhtml_tree_list_init : pmyhtml_tree_list_t; cdecl; external MyHTMLLib;
procedure myhtml_tree_list_clean (list : pmyhtml_tree_list_t); cdecl;
  external MyHTMLLib;
function myhtml_tree_list_destroy (list : pmyhtml_tree_list_t; destroy_self :
  Boolean) : pmyhtml_tree_list_t; cdecl; external MyHTMLLib;
procedure myhtml_tree_list_append (list : pmyhtml_tree_list_t; node :
  pmyhtml_tree_node_t); cdecl; external MyHTMLLib;
procedure myhtml_tree_list_append_after_index (list : pmyhtml_tree_list_t;
  node : pmyhtml_tree_node_t; index : QWord); cdecl; external MyHTMLLib;
procedure myhtml_tree_list_insert_by_index (list : pmyhtml_tree_list_t;
  node : pmyhtml_tree_node_t; index : QWord); cdecl; external MyHTMLLib;
function myhtml_tree_list_current_node (list : pmyhtml_tree_list_t) :
  pmyhtml_tree_node_t; cdecl; external MyHTMLLib;

(* token list *)
function myhtml_tree_token_list_init : pmyhtml_tree_token_list_t; cdecl;
  external MyHTMLLib;
procedure myhtml_tree_token_list_clean (list : pmyhtml_tree_token_list_t);
  cdecl; external MyHTMLLib;
function myhtml_tree_token_list_destroy (list : pmyhtml_tree_token_list_t;
  destroy_self : Boolean) : pmyhtml_tree_token_list_t; cdecl;
  external MyHTMLLib;
procedure myhtml_tree_token_list_append (list : pmyhtml_tree_token_list_t;
  token : pmyhtml_token_node_t); cdecl; external MyHTMLLib;
procedure myhtml_tree_token_list_append_after_index (list :
  pmyhtml_tree_token_list_t; token : pmyhtml_token_node_t; index : QWord);
  cdecl; external MyHTMLLib;
function myhtml_tree_token_list_current_node (list : pmyhtml_tree_token_list_t)
  : pmyhtml_token_node_t; cdecl; external MyHTMLLib;

(* active formatting *)
function myhtml_tree_active_formatting_init (tree : pmyhtml_tree_t) :
  pmyhtml_tree_list_t; cdecl; external MyHTMLLib;
procedure myhtml_tree_active_formatting_clean (tree : pmyhtml_tree_t); cdecl;
  external MyHTMLLib;
function myhtml_tree_active_formatting_destroy (tree : pmyhtml_tree_t) :
  pmyhtml_tree_list_t; cdecl; external MyHTMLLib;
function myhtml_tree_active_formatting_is_marker (tree : pmyhtml_tree_t; idx :
  pmyhtml_tree_node_t) : Boolean; cdecl; external MyHTMLLib;
function myhtml_tree_active_formatting_between_last_marker (tree :
  pmyhtml_tree_t; tag_idx : myhtml_tag_id_t; return_idx : PQWord) :
  pmyhtml_tree_node_t; cdecl; external MyHTMLLib;
procedure myhtml_tree_active_formatting_append (tree : pmyhtml_tree_t;
  node : pmyhtml_tree_node_t); cdecl; external MyHTMLLib;
procedure myhtml_tree_active_formatting_append_with_check (tree :
  pmyhtml_tree_t; node : pmyhtml_tree_node_t); cdecl; external MyHTMLLib;
procedure myhtml_tree_active_formatting_pop (tree : pmyhtml_tree_t); cdecl;
  external MyHTMLLib;
procedure myhtml_tree_active_formatting_remove (tree : pmyhtml_tree_t; node :
  pmyhtml_tree_node_t); cdecl; external MyHTMLLib;
procedure myhtml_tree_active_formatting_remove_by_index (tree :pmyhtml_tree_t;
  idx : QWord); cdecl; external MyHTMLLib;
procedure myhtml_tree_active_formatting_reconstruction (tree : pmyhtml_tree_t);
  cdecl; external MyHTMLLib;
procedure myhtml_tree_active_formatting_up_to_last_marker (tree :
  pmyhtml_tree_t); cdecl; external MyHTMLLib;
function myhtml_tree_active_formatting_find (tree : pmyhtml_tree_t; idx :
  pmyhtml_tree_node_t; return_idx : PQWord) : Boolean; cdecl;
  external MyHTMLLib;
function myhtml_tree_active_formatting_current_node (tree : pmyhtml_tree_t) :
  pmyhtml_tree_node_t; cdecl; external MyHTMLLib;

(* open elements *)
function myhtml_tree_open_elements_init (tree : pmyhtml_tree_t) :
  pmyhtml_tree_list_t; cdecl; external MyHTMLLib;
procedure myhtml_tree_open_elements_clean (tree : pmyhtml_tree_t); cdecl;
  external MyHTMLLib;
function myhtml_tree_open_elements_destroy (tree : pmyhtml_tree_t) :
  pmyhtml_tree_list_t; cdecl; external MyHTMLLib;
function myhtml_tree_current_node (tree : pmyhtml_tree_t) : pmyhtml_tree_node_t;
  cdecl; external MyHTMLLib;
function myhtml_tree_adjusted_current_node (tree : pmyhtml_tree_t) :
  pmyhtml_tree_node_t; cdecl; external MyHTMLLib;
procedure myhtml_tree_open_elements_append (tree : pmyhtml_tree_t; node :
  pmyhtml_tree_node_t); cdecl; external MyHTMLLib;
procedure myhtml_tree_open_elements_append_after_index (tree : pmyhtml_tree_t;
  node : pmyhtml_tree_node_t; index : QWord); cdecl; external MyHTMLLib;
procedure myhtml_tree_open_elements_pop (tree : pmyhtml_tree_t); cdecl;
  external MyHTMLLib;
procedure myhtml_tree_open_elements_pop_until (tree : pmyhtml_tree_t; tag_idx :
  myhtml_tag_id_t; mynamespace : myhtml_namespace_t; is_exclude : Boolean);
  cdecl; external MyHTMLLib;
procedure myhtml_tree_open_elements_pop_until_by_node (tree : pmyhtml_tree_t;
  node_idx : pmyhtml_tree_node_t; is_exclude : Boolean); cdecl;
  external MyHTMLLib;
procedure myhtml_tree_open_elements_pop_until_by_index (tree : pmyhtml_tree_t;
  idx : QWord; is_exclude : Boolean); cdecl; external MyHTMLLib;
procedure myhtml_tree_open_elements_remove (tree : pmyhtml_tree_t; node :
  pmyhtml_tree_node_t); cdecl; external MyHTMLLib;
function myhtml_tree_open_elements_find (tree : pmyhtml_tree_t; idx :
  pmyhtml_tree_node_t; pos : PQWord) : Boolean; cdecl; external MyHTMLLib;
function myhtml_tree_open_elements_find_reverse (tree : pmyhtml_tree_t; idx :
  pmyhtml_tree_node_t; pos : PQWord) : Boolean; cdecl; external MyHTMLLib;
function myhtml_tree_open_elements_find_by_tag_idx (tree : pmyhtml_tree_t;
  tag_idx : myhtml_tag_id_t; mynamespace : myhtml_namespace_t; return_index :
  PQWord) : pmyhtml_tree_node_t; cdecl; external MyHTMLLib;
function myhtml_tree_open_elements_find_by_tag_idx_reverse (tree :
  pmyhtml_tree_t; tag_idx : myhtml_tag_id_t; mynamespace : myhtml_namespace_t;
  return_index : PQWord) : pmyhtml_tree_node_t; cdecl; external MyHTMLLib;
function myhtml_tree_element_in_scope (tree : pmyhtml_tree_t; tag_idx :
  myhtml_tag_id_t; mynamespace : myhtml_namespace_t; category :
  myhtml_tag_categories_t) : pmyhtml_tree_node_t; cdecl; external MyHTMLLib;
function myhtml_tree_element_in_scope_by_node (node : pmyhtml_tree_node_t;
  category : myhtml_tag_categories_t) : Boolean; cdecl; external MyHTMLLib;
procedure myhtml_tree_generate_implied_end_tags (tree : pmyhtml_tree_t;
  exclude_tag_idx : myhtml_tag_id_t; mynamespace : myhtml_namespace_t); cdecl;
  external MyHTMLLib;
procedure myhtml_tree_generate_all_implied_end_tags (tree : pmyhtml_tree_t;
  exclude_tag_idx : myhtml_tag_id_t; mynamespace : myhtml_namespace_t); cdecl;
  external MyHTMLLib;
function myhtml_tree_appropriate_place_inserting (tree : pmyhtml_tree_t;
  override_target : pmyhtml_tree_node_t; mode : pmyhtml_tree_insertion_mode_t) :
  pmyhtml_tree_node_t; cdecl; external MyHTMLLib;
function myhtml_tree_appropriate_place_inserting_in_tree (target :
  pmyhtml_tree_node_t; mode : pmyhtml_tree_insertion_mode_t) :
  pmyhtml_tree_node_t; cdecl; external MyHTMLLib;

(* template insertion *)
function myhtml_tree_template_insertion_init (tree : pmyhtml_tree_t) :
  pmyhtml_tree_insertion_list_t; cdecl; external MyHTMLLib;
procedure myhtml_tree_template_insertion_clean (tree : pmyhtml_tree_t); cdecl;
  external MyHTMLLib;
function myhtml_tree_template_insertion_destroy (tree : pmyhtml_tree_t) :
  pmyhtml_tree_insertion_list_t; cdecl; external MyHTMLLib;
procedure myhtml_tree_template_insertion_append (tree : pmyhtml_tree_t;
  insert_mode : myhtml_insertion_mode_t); cdecl; external MyHTMLLib;
procedure myhtml_tree_template_insertion_pop (tree : pmyhtml_tree_t); cdecl;
  external MyHTMLLib;
procedure myhtml_tree_reset_insertion_mode_appropriately (tree :
  pmyhtml_tree_t); cdecl; external MyHTMLLib;
function myhtml_tree_adoption_agency_algorithm (tree : pmyhtml_tree_t; token :
  pmyhtml_token_node_t; subject_tag_idx : myhtml_tag_id_t) : Boolean; cdecl;
  external MyHTMLLib;
function myhtml_tree_template_insertion_length (tree : pmyhtml_tree_t) : QWord;
  cdecl; external MyHTMLLib;

(* other for a tree *)
function myhtml_tree_node_create (tree : pmyhtml_tree_t) : pmyhtml_tree_node_t;
  cdecl; external MyHTMLLib;
procedure myhtml_tree_node_delete (node : pmyhtml_tree_node_t); cdecl;
  external MyHTMLLib;
procedure myhtml_tree_node_delete_recursive (node : pmyhtml_tree_node_t); cdecl;
  external MyHTMLLib;
procedure myhtml_tree_node_clean (tree_node : pmyhtml_tree_node_t); cdecl;
  external MyHTMLLib;
procedure myhtml_tree_node_free (node : pmyhtml_tree_node_t); cdecl;
  external MyHTMLLib;
function myhtml_tree_node_clone (node : pmyhtml_tree_node_t) :
  pmyhtml_tree_node_t; cdecl; external MyHTMLLib;
procedure myhtml_tree_node_add_child (root : pmyhtml_tree_node_t; node :
  pmyhtml_tree_node_t); cdecl; external MyHTMLLib;
procedure myhtml_tree_node_insert_before (root : pmyhtml_tree_node_t; node :
  pmyhtml_tree_node_t); cdecl; external MyHTMLLib;
procedure myhtml_tree_node_insert_after (root : pmyhtml_tree_node_t; node :
  pmyhtml_tree_node_t); cdecl; external MyHTMLLib;
procedure myhtml_tree_node_insert_by_mode (adjusted_location :
  pmyhtml_tree_node_t; node : pmyhtml_tree_node_t; mode :
  myhtml_tree_insertion_mode_t); cdecl; external MyHTMLLib;
function myhtml_tree_node_remove (node : pmyhtml_tree_node_t) :
  pmyhtml_tree_node_t; cdecl; external MyHTMLLib;
function myhtml_tree_node_insert_html_element (tree : pmyhtml_tree_t; token :
  pmyhtml_token_node_t) : pmyhtml_tree_node_t; cdecl; external MyHTMLLib;
function myhtml_tree_node_insert_foreign_element (tree : pmyhtml_tree_t; token :
  pmyhtml_token_node_t) : pmyhtml_tree_node_t; cdecl; external MyHTMLLib;
function myhtml_tree_node_insert_by_token (tree : pmyhtml_tree_t; token :
  pmyhtml_token_node_t; ns : myhtml_namespace_t) : pmyhtml_tree_node_t; cdecl;
  external MyHTMLLib;
function myhtml_tree_node_insert (tree : pmyhtml_tree_t; tag_idx :
  myhtml_tag_id_t; ns : myhtml_namespace_t) : pmyhtml_tree_node_t; cdecl;
  external MyHTMLLib;
function myhtml_tree_node_insert_by_node (tree : pmyhtml_tree_t; idx :
  pmyhtml_tree_node_t) : pmyhtml_tree_node_t; cdecl; external MyHTMLLib;
function myhtml_tree_node_insert_comment (tree : pmyhtml_tree_t; token :
  pmyhtml_token_node_t; parent : pmyhtml_tree_node_t) : pmyhtml_tree_node_t;
  cdecl; external MyHTMLLib;
function myhtml_tree_node_insert_doctype (tree : pmyhtml_tree_t; token :
  pmyhtml_token_node_t) : pmyhtml_tree_node_t; cdecl; external MyHTMLLib;
function myhtml_tree_node_insert_root (tree : pmyhtml_tree_t; token :
  pmyhtml_token_node_t; ns : myhtml_namespace_t) : pmyhtml_tree_node_t; cdecl;
  external MyHTMLLib;
function myhtml_tree_node_insert_text (tree : pmyhtml_tree_t; token :
  pmyhtml_token_node_t) : pmyhtml_tree_node_t; cdecl; external MyHTMLLib;
function myhtml_tree_node_find_parent_by_tag_id (node : pmyhtml_tree_node_t;
  tag_id : myhtml_tag_id_t) : pmyhtml_tree_node_t; cdecl; external MyHTMLLib;

(* other *)
procedure myhtml_tree_wait_for_last_done_token (tree : pmyhtml_tree_t;
  token_for_wait : pmyhtml_token_node_t); cdecl; external MyHTMLLib;
procedure myhtml_tree_tags_close_p (tree : pmyhtml_tree_t; token :
  pmyhtml_token_node_t); cdecl; external MyHTMLLib;
function myhtml_tree_generic_raw_text_element_parsing_algorithm (tree :
  pmyhtml_tree_t; token_node : pmyhtml_token_node_t) : pmyhtml_tree_node_t;
  cdecl; external MyHTMLLib;
procedure myhtml_tree_clear_stack_back_table_context (tree : pmyhtml_tree_t);
  cdecl; external MyHTMLLib;
procedure myhtml_tree_clear_stack_back_table_body_context (tree :
  pmyhtml_tree_t); cdecl; external MyHTMLLib;
procedure myhtml_tree_clear_stack_back_table_row_context (tree :
  pmyhtml_tree_t); cdecl; external MyHTMLLib;
procedure myhtml_tree_close_cell (tree : pmyhtml_tree_t; tr_or_th_node :
  pmyhtml_tree_node_t; token : pmyhtml_token_node_t); cdecl; external MyHTMLLib;
function myhtml_tree_is_mathml_intergation_point (tree : pmyhtml_tree_t; node :
  pmyhtml_tree_node_t) : Boolean; cdecl; external MyHTMLLib;
function myhtml_tree_is_html_integration_point (tree : pmyhtml_tree_t; node :
  pmyhtml_tree_node_t) : Boolean; cdecl; external MyHTMLLib;

(* temp tag name *)
function myhtml_tree_temp_tag_name_init (temp_tag_name :
  pmyhtml_tree_temp_tag_name_t) : mystatus_t; cdecl; external MyHTMLLib;
procedure myhtml_tree_temp_tag_name_clean (temp_tag_name :
  pmyhtml_tree_temp_tag_name_t); cdecl; external MyHTMLLib;
function myhtml_tree_temp_tag_name_destroy (temp_tag_name :
  pmyhtml_tree_temp_tag_name_t; self_destroy : Boolean) :
  pmyhtml_tree_temp_tag_name_t; cdecl; external MyHTMLLib;
function myhtml_tree_temp_tag_name_append (temp_tag_name :
  pmyhtml_tree_temp_tag_name_t; const name : PChar; name_len : QWord) :
  mystatus_t; cdecl; external MyHTMLLib;
function myhtml_tree_temp_tag_name_append_one (temp_tag_name :
  pmyhtml_tree_temp_tag_name_t; const name : Char) : mystatus_t; cdecl;
  external MyHTMLLib;

(* special token list *)
function myhtml_tree_special_list_init (special :
  pmyhtml_tree_special_token_list_t) : mystatus_t; cdecl; external MyHTMLLib;
function myhtml_tree_special_list_append (special :
  pmyhtml_tree_special_token_list_t; token : pmyhtml_token_node_t; ns :
  myhtml_namespace_t) : mystatus_t; cdecl; external MyHTMLLib;
function myhtml_tree_special_list_length (special :
  pmyhtml_tree_special_token_list_t) : QWord; cdecl; external MyHTMLLib;
function myhtml_tree_special_list_get_last (special :
  pmyhtml_tree_special_token_list_t) : pmyhtml_tree_special_token_t; cdecl;
  external MyHTMLLib;
function myhtml_tree_special_list_pop (special :
  pmyhtml_tree_special_token_list_t) : QWord; cdecl; external MyHTMLLib;

(* incoming buffer *)
function myhtml_tree_incoming_buffer_first (tree : pmyhtml_tree_t) :
  pmycore_incoming_buffer_t; cdecl; external MyHTMLLib;
function myhtml_tree_incoming_buffer_make_data (tree : pmyhtml_tree_t;
  start : QWord; length : QWord) : PChar; cdecl; external MyHTMLLib;

(*myhtml/myosi.h***************************************************************)

type
  (* parser state function *)
  myhtml_tokenizer_state_f = function (tree : pmyhtml_tree_t; token_node :
    pmyhtml_token_node_t; const html : PChar; html_offset : QWord; html_size :
    QWord) : QWord of object;

  (* parser insertion mode function *)
  myhtml_insertion_f = function (tree : pmyhtml_tree_t; token :
    pmyhtml_token_node_t) : Boolean of object;

  (* char references state *)
  myhtml_data_process_state_f = function (chref : pmyhtml_data_process_entry_t;
    str : pmycore_string_t; const data : PChar; offset : QWord; size : QWord) :
    QWord of object;

  (* callback function *)
  myhtml_callback_token_f = function (tree : pmyhtml_tree_t; token :
    pmyhtml_token_node_t; ctx : Pointer) : Pointer of object;
  myhtml_callback_tree_node_f = procedure (tree : pmyhtml_tree_t; node :
    pmyhtml_tree_node_t; ctx : Pointer) of object;

  (* find attribute value functions *)
  myhtml_attribute_value_find_f = function (str_key : pmycore_string_t;
    const value : PChar; value_len : QWord) : Boolean of object;

(*myhtml/token.h***************************************************************)

type
  pmyhtml_token_t = ^myhtml_token_t;
  myhtml_token_t = record
    tree : pmyhtml_tree_t; { ref }

    nodes_obj : pmcobject_async_t; { myhtml_token_node_t }
    attr_obj : pmcobject_async_t; { myhtml_token_attr_t }

    (* def thread node_id *)
    mcasync_token_id : QWord;
    mcasync_attr_id : QWord;

    is_new_tmp : Boolean;
  end;

function myhtml_token_create (tree : pmyhtml_tree_t; size : QWord) :
  pmyhtml_token_t; cdecl; external MyHTMLLib;
procedure myhtml_token_clean (token : pmyhtml_token_t); cdecl;
  external MyHTMLLib;
procedure myhtml_token_clean_all (token : pmyhtml_token_t); cdecl;
  external MyHTMLLib;
function myhtml_token_destroy (token : pmyhtml_token_t) : pmyhtml_token_t;
  cdecl; external MyHTMLLib;
function myhtml_token_node_tag_id (token_node : pmyhtml_token_node_t) :
  myhtml_tag_id_t; cdecl; external MyHTMLLib;
function myhtml_token_node_raw_position (token_node : pmyhtml_token_node_t) :
  myhtml_position_t; cdecl; external MyHTMLLib;
function myhtml_token_node_element_position (token_node : pmyhtml_token_node_t)
  : myhtml_position_t; cdecl; external MyHTMLLib;
function myhtml_token_node_attribute_first (token_node : pmyhtml_token_node_t) :
  pmyhtml_tree_attr_t; cdecl; external MyHTMLLib;
function myhtml_token_node_attribute_last (token_node : pmyhtml_token_node_t) :
  pmyhtml_tree_attr_t; cdecl; external MyHTMLLib;
function myhtml_token_node_text (token_node : pmyhtml_token_node_t; length :
  PQWord) : PChar; cdecl; external MyHTMLLib;
function myhtml_token_node_string (token_node : pmyhtml_token_node_t) :
  pmycore_string_t; cdecl; external MyHTMLLib;
function myhtml_token_node_is_close (token_node : pmyhtml_token_node_t) :
  Boolean; cdecl; external MyHTMLLib;
function myhtml_token_node_is_close_self (token_node : pmyhtml_token_node_t) :
  Boolean; cdecl; external MyHTMLLib;
function myhtml_token_node_create (token : pmyhtml_token_t; async_node_id :
  QWord) : pmyhtml_token_node_t; cdecl; external MyHTMLLib;
procedure myhtml_token_node_clean (node : pmyhtml_token_node_t); cdecl;
  external MyHTMLLib;
function myhtml_token_attr_create (token : pmyhtml_token_t; async_node_id :
  QWord) : pmyhtml_token_attr_t; cdecl; external MyHTMLLib;
procedure myhtml_token_attr_clean (attr : pmyhtml_token_attr_t); cdecl;
  external MyHTMLLib;
function myhtml_token_attr_remove (node : pmyhtml_token_node_t; attr :
  pmyhtml_token_attr_t) : pmyhtml_token_attr_t; cdecl; external MyHTMLLib;
function myhtml_token_attr_remove_by_name (node : pmyhtml_token_node_t;
  const name : PChar; name_length : QWord) : pmyhtml_token_attr_t; cdecl;
  external MyHTMLLib;
procedure myhtml_token_attr_delete_all (token : pmyhtml_token_t; node :
  pmyhtml_token_node_t); cdecl; external MyHTMLLib;
procedure myhtml_token_delete (token : pmyhtml_token_t; node :
  pmyhtml_token_node_t); cdecl; external MyHTMLLib;
procedure myhtml_token_node_wait_for_done (token : pmyhtml_token_t; node :
  pmyhtml_token_node_t); cdecl; external MyHTMLLib;
procedure myhtml_token_set_done (node : pmyhtml_token_node_t); cdecl;
  external MyHTMLLib;
function myhtml_token_attr_match (token : myhtml_token_t; target :
  pmyhtml_token_node_t; const key : PChar; key_size : QWord; const value :
  PChar; value_size : QWord) : pmyhtml_token_attr_t; cdecl; external MyHTMLLib;
function myhtml_token_attr_match_case (token : pmyhtml_token_t; target :
  pmyhtml_token_node_t; const key : PChar; key_size : QWord; const value :
  PChar; value_size : QWord) : pmyhtml_token_attr_t; cdecl; external MyHTMLLib;
function myhtml_token_release_and_check_doctype_attributes (token :
  pmyhtml_token_t; target : pmyhtml_token_node_t; return_doctype :
  pmyhtml_tree_doctype_t) : Boolean; cdecl; external MyHTMLLib;
procedure myhtml_token_adjust_mathml_attributes (target : pmyhtml_token_node_t);
  cdecl; external MyHTMLLib;
procedure myhtml_token_adjust_svg_attributes (target : pmyhtml_token_node_t);
  cdecl; external MyHTMLLib;
procedure myhtml_token_adjust_foreign_attributes (target :
  pmyhtml_token_node_t); cdecl; external MyHTMLLib;
function myhtml_token_node_attr_append (token : pmyhtml_token_t; dest :
  pmyhtml_token_node_t; const key : PChar; key_len : QWord; const value : PChar;
  value_len : QWord; thread_idx : QWord) : pmyhtml_token_attr_t; cdecl;
  external MyHTMLLib;
function myhtml_token_node_attr_append_with_convert_encoding (token :
  pmyhtml_token_t; dest : pmyhtml_token_node_t; const key : PChar; key_len :
  QWord; const value : PChar; value_len : QWord; thread_idx : QWord; encoding :
  myencoding_t) : pmyhtml_token_attr_t; cdecl; external MyHTMLLib;
procedure myhtml_token_node_text_append (token : pmyhtml_token_t; dest :
  pmyhtml_token_node_t; const text : PChar; text_len : QWord); cdecl;
  external MyHTMLLib;
procedure myhtml_token_node_attr_copy (token : pmyhtml_token_t; target :
  pmyhtml_token_node_t; dest : pmyhtml_token_node_t; thread_idx : QWord); cdecl;
  external MyHTMLLib;
procedure myhtml_token_node_attr_copy_with_check (token : pmyhtml_token_t;
  target : pmyhtml_token_node_t; dest : pmyhtml_token_node_t; thread_idx :
  QWord); cdecl; external MyHTMLLib;
function myhtml_token_node_clone (token : pmyhtml_token_t; node :
  pmyhtml_token_node_t; token_thread_idx : QWord; attr_thread_idx : QWord) :
  pmyhtml_token_node_t; cdecl; external MyHTMLLib;
function myhtml_token_attr_copy (token : pmyhtml_token_t; attr :
  pmyhtml_token_attr_t; dest : pmyhtml_token_node_t; thread_idx : QWord) :
  Boolean; cdecl; external MyHTMLLib;
function myhtml_token_attr_by_name (node : pmyhtml_token_node_t; const name :
  PChar; name_size : QWord) : pmyhtml_token_attr_t; cdecl; external MyHTMLLib;
function myhtml_token_attr_compare (target : pmyhtml_token_node_t; dest :
  pmyhtml_token_node_t) : Boolean; cdecl; external MyHTMLLib;
function myhtml_token_merge_two_token_string (tree : pmyhtml_tree_t; token_to :
  pmyhtml_token_node_t; token_from : pmyhtml_token_node_t; cp_reverse :
  Boolean) : pmyhtml_token_node_t; cdecl; external MyHTMLLib;
procedure myhtml_token_set_replacement_character_for_null_token (tree :
  pmyhtml_tree_t; node : pmyhtml_token_node_t); cdecl; external MyHTMLLib;


implementation

end.

