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
function mcsimple_init_list_entries (mcsimple : pmcimple_t; pos : QWord)
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
procedure mchar_async_node_delete (mchar_async : pmchar_async; node_idx :
  QWord); cdecl; external MyHTMLLib;
function mchar_async_chunk_malloc (mchar_async : pmchar_async; node :
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

(*mycore/utils/resources.h*****************************************************)

const
  MyCORE_STRING_MAP_CHAR_OTHER                                          = #$000;
  MyCORE_STRING_MAP_CHAR_A_Z_a_z                                        = #$001;
  MyCORE_STRING_MAP_CHAR_WHITESPACE                                     = #$002;

  mycore_string_chars_num_map : array [0 .. 255] of Byte = (
    $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,
    $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,
    $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,
    $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,
    $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,
    $ff,  $ff,  $ff,  $00,  $01,  $02,  $03,  $04,  $05,
    $06,  $07,  $08,  $09,  $ff,  $ff,  $ff,  $ff,  $ff,
    $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,
    $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,
    $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,
    $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,
    $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,
    $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,
    $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,
    $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,
    $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,
    $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,
    $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,
    $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,
    $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,
    $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,
    $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,
    $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,
    $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,
    $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,
    $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,
    $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,
    $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,
    $ff,  $ff,  $ff,  $ff
  );

  mycore_string_chars_hex_map : array [0 .. 255] of Byte = (
    $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,
    $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,
    $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,
    $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,
    $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,
    $ff,  $ff,  $ff,  $00,  $01,  $02,  $03,  $04,  $05,
    $06,  $07,  $08,  $09,  $ff,  $ff,  $ff,  $ff,  $ff,
    $ff,  $ff,  $0a,  $0b,  $0c,  $0d,  $0e,  $0f,  $ff,
    $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,
    $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,
    $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $0a,  $0b,
    $0c,  $0d,  $0e,  $0f,  $ff,  $ff,  $ff,  $ff,  $ff,
    $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,
    $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,
    $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,
    $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,
    $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,
    $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,
    $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,
    $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,
    $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,
    $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,
    $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,
    $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,
    $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,
    $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,
    $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,
    $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,
    $ff,  $ff,  $ff,  $ff
  );

  mycore_string_chars_lowercase_map : array [0 .. 255] of Byte = (
    $00,  $01,  $02,  $03,  $04,  $05,  $06,  $07,  $08,
    $09,  $0a,  $0b,  $0c,  $0d,  $0e,  $0f,  $10,  $11,
    $12,  $13,  $14,  $15,  $16,  $17,  $18,  $19,  $1a,
    $1b,  $1c,  $1d,  $1e,  $1f,  $20,  $21,  $22,  $23,
    $24,  $25,  $26,  $27,  $28,  $29,  $2a,  $2b,  $2c,
    $2d,  $2e,  $2f,  $30,  $31,  $32,  $33,  $34,  $35,
    $36,  $37,  $38,  $39,  $3a,  $3b,  $3c,  $3d,  $3e,
    $3f,  $40,  $61,  $62,  $63,  $64,  $65,  $66,  $67,
    $68,  $69,  $6a,  $6b,  $6c,  $6d,  $6e,  $6f,  $70,
    $71,  $72,  $73,  $74,  $75,  $76,  $77,  $78,  $79,
    $7a,  $5b,  $5c,  $5d,  $5e,  $5f,  $60,  $61,  $62,
    $63,  $64,  $65,  $66,  $67,  $68,  $69,  $6a,  $6b,
    $6c,  $6d,  $6e,  $6f,  $70,  $71,  $72,  $73,  $74,
    $75,  $76,  $77,  $78,  $79,  $7a,  $7b,  $7c,  $7d,
    $7e,  $7f,  $80,  $81,  $82,  $83,  $84,  $85,  $86,
    $87,  $88,  $89,  $8a,  $8b,  $8c,  $8d,  $8e,  $8f,
    $90,  $91,  $92,  $93,  $94,  $95,  $96,  $97,  $98,
    $99,  $9a,  $9b,  $9c,  $9d,  $9e,  $9f,  $a0,  $a1,
    $a2,  $a3,  $a4,  $a5,  $a6,  $a7,  $a8,  $a9,  $aa,
    $ab,  $ac,  $ad,  $ae,  $af,  $b0,  $b1,  $b2,  $b3,
    $b4,  $b5,  $b6,  $b7,  $b8,  $b9,  $ba,  $bb,  $bc,
    $bd,  $be,  $bf,  $c0,  $c1,  $c2,  $c3,  $c4,  $c5,
    $c6,  $c7,  $c8,  $c9,  $ca,  $cb,  $cc,  $cd,  $ce,
    $cf,  $d0,  $d1,  $d2,  $d3,  $d4,  $d5,  $d6,  $d7,
    $d8,  $d9,  $da,  $db,  $dc,  $dd,  $de,  $df,  $e0,
    $e1,  $e2,  $e3,  $e4,  $e5,  $e6,  $e7,  $e8,  $e9,
    $ea,  $eb,  $ec,  $ed,  $ee,  $ef,  $f0,  $f1,  $f2,
    $f3,  $f4,  $f5,  $f6,  $f7,  $f8,  $f9,  $fa,  $fb,
    $fc,  $fd,  $fe,  $ff
  );

  mycore_string_chars_uppercase_map : array [0 .. 255] of Byte = (
    $00,  $01,  $02,  $03,  $04,  $05,  $06,  $07,  $08,
    $09,  $0a,  $0b,  $0c,  $0d,  $0e,  $0f,  $10,  $11,
    $12,  $13,  $14,  $15,  $16,  $17,  $18,  $19,  $1a,
    $1b,  $1c,  $1d,  $1e,  $1f,  $20,  $21,  $22,  $23,
    $24,  $25,  $26,  $27,  $28,  $29,  $2a,  $2b,  $2c,
    $2d,  $2e,  $2f,  $30,  $31,  $32,  $33,  $34,  $35,
    $36,  $37,  $38,  $39,  $3a,  $3b,  $3c,  $3d,  $3e,
    $3f,  $40,  $41,  $42,  $43,  $44,  $45,  $46,  $47,
    $48,  $49,  $4a,  $4b,  $4c,  $4d,  $4e,  $4f,  $50,
    $51,  $52,  $53,  $54,  $55,  $56,  $57,  $58,  $59,
    $5a,  $5b,  $5c,  $5d,  $5e,  $5f,  $60,  $41,  $42,
    $43,  $44,  $45,  $46,  $47,  $48,  $49,  $4a,  $4b,
    $4c,  $4d,  $4e,  $4f,  $50,  $51,  $52,  $53,  $54,
    $55,  $56,  $57,  $58,  $59,  $5a,  $7b,  $7c,  $7d,
    $7e,  $7f,  $80,  $81,  $82,  $83,  $84,  $85,  $86,
    $87,  $88,  $89,  $8a,  $8b,  $8c,  $8d,  $8e,  $8f,
    $90,  $91,  $92,  $93,  $94,  $95,  $96,  $97,  $98,
    $99,  $9a,  $9b,  $9c,  $9d,  $9e,  $9f,  $a0,  $a1,
    $a2,  $a3,  $a4,  $a5,  $a6,  $a7,  $a8,  $a9,  $aa,
    $ab,  $ac,  $ad,  $ae,  $af,  $b0,  $b1,  $b2,  $b3,
    $b4,  $b5,  $b6,  $b7,  $b8,  $b9,  $ba,  $bb,  $bc,
    $bd,  $be,  $bf,  $c0,  $c1,  $c2,  $c3,  $c4,  $c5,
    $c6,  $c7,  $c8,  $c9,  $ca,  $cb,  $cc,  $cd,  $ce,
    $cf,  $d0,  $d1,  $d2,  $d3,  $d4,  $d5,  $d6,  $d7,
    $d8,  $d9,  $da,  $db,  $dc,  $dd,  $de,  $df,  $e0,
    $e1,  $e2,  $e3,  $e4,  $e5,  $e6,  $e7,  $e8,  $e9,
    $ea,  $eb,  $ec,  $ed,  $ee,  $ef,  $f0,  $f1,  $f2,
    $f3,  $f4,  $f5,  $f6,  $f7,  $f8,  $f9,  $fa,  $fb,
    $fc,  $fd,  $fe,  $ff
  );

  replacement_character : array [0 .. 159] of QWord = (
    65535,      1,      2,      3,      4,      5,      6,      7,      8,
        9,     10,     11,     12,     13,     14,     15,     16,     17,
       18,     19,     20,     21,     22,     23,     24,     25,     26,
       27,     28,     29,     30,     31,     32,     33,     34,     35,
       36,     37,     38,     39,     40,     41,     42,     43,     44,
       45,     46,     47,     48,     49,     50,     51,     52,     53,
       54,     55,     56,     57,     58,     59,     60,     61,     62,
       63,     64,     65,     66,     67,     68,     69,     70,     71,
       72,     73,     74,     75,     76,     77,     78,     79,     80,
       81,     82,     83,     84,     85,     86,     87,     88,     89,
       90,     91,     92,     93,     94,     95,     96,     97,     98,
       99,    100,    101,    102,    103,    104,    105,    106,    107,
      108,    109,    110,    111,    112,    113,    114,    115,    116,
      117,    118,    119,    120,    121,    122,    123,    124,    125,
      126,    127,   8364,    129,   8218,    402,   8222,   8230,   8224,
     8225,    710,   8240,    352,   8249,    338,    141,    381,    143,
      144,   8216,   8217,   8220,   8221,   8226,   8211,   8212,    732,
     8482,    353,   8250,    339,    157,    382,    376
  );

  mycore_string_alphanumeric_character : array [0 .. 255] of Byte = (
    $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,
    $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,
    $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,
    $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,
    $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,
    $ff,  $ff,  $ff,  $00,  $01,  $02,  $03,  $04,  $05,
    $06,  $07,  $08,  $09,  $ff,  $ff,  $ff,  $ff,  $ff,
    $ff,  $ff,  $0a,  $0b,  $0c,  $0d,  $0e,  $0f,  $0a,
    $0b,  $0c,  $0d,  $0e,  $0f,  $0a,  $0b,  $0c,  $0d,
    $0e,  $0f,  $0a,  $0b,  $0c,  $0d,  $0e,  $0f,  $0a,
    $0b,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $0c,  $0d,
    $0e,  $0f,  $0a,  $0b,  $0c,  $0d,  $0e,  $0f,  $0a,
    $0b,  $0c,  $0d,  $0e,  $0f,  $0a,  $0b,  $0c,  $0d,
    $0e,  $0f,  $0a,  $0b,  $0c,  $0d,  $ff,  $ff,  $ff,
    $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,
    $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,
    $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,
    $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,
    $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,
    $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,
    $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,
    $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,
    $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,
    $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,
    $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,
    $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,
    $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,
    $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,
    $ff,  $ff,  $ff,  $ff
  );

  mycore_string_alpha_character : array [0 .. 255] of Byte = (
    $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,
    $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,
    $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,
    $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,
    $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,
    $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,
    $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,
    $ff,  $ff,  $0a,  $0b,  $0c,  $0d,  $0e,  $0f,  $0a,
    $0b,  $0c,  $0d,  $0e,  $0f,  $0a,  $0b,  $0c,  $0d,
    $0e,  $0f,  $0a,  $0b,  $0c,  $0d,  $0e,  $0f,  $0a,
    $0b,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $0c,  $0d,
    $0e,  $0f,  $0a,  $0b,  $0c,  $0d,  $0e,  $0f,  $0a,
    $0b,  $0c,  $0d,  $0e,  $0f,  $0a,  $0b,  $0c,  $0d,
    $0e,  $0f,  $0a,  $0b,  $0c,  $0d,  $ff,  $ff,  $ff,
    $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,
    $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,
    $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,
    $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,
    $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,
    $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,
    $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,
    $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,
    $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,
    $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,
    $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,
    $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,
    $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,
    $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,
    $ff,  $ff,  $ff,  $ff
  );

  mycore_tokenizer_chars_map : array [0 .. 255] of Byte = (
    $00,  $00,  $00,  $00,  $00,  $00,  $00,  $00,  $00,
    $02,  $02,  $00,  $02,  $02,  $00,  $00,  $00,  $00,
    $00,  $00,  $00,  $00,  $00,  $00,  $00,  $00,  $00,
    $00,  $00,  $00,  $00,  $00,  $02,  $00,  $00,  $00,
    $00,  $00,  $00,  $00,  $00,  $00,  $00,  $00,  $00,
    $00,  $00,  $00,  $00,  $00,  $00,  $00,  $00,  $00,
    $00,  $00,  $00,  $00,  $00,  $00,  $00,  $00,  $00,
    $00,  $00,  $01,  $01,  $01,  $01,  $01,  $01,  $01,
    $01,  $01,  $01,  $01,  $01,  $01,  $01,  $01,  $01,
    $01,  $01,  $01,  $01,  $01,  $01,  $01,  $01,  $01,
    $01,  $00,  $00,  $00,  $00,  $00,  $00,  $01,  $01,
    $01,  $01,  $01,  $01,  $01,  $01,  $01,  $01,  $01,
    $01,  $01,  $01,  $01,  $01,  $01,  $01,  $01,  $01,
    $01,  $01,  $01,  $01,  $01,  $01,  $00,  $00,  $00,
    $00,  $00,  $00,  $00,  $00,  $00,  $00,  $00,  $00,
    $00,  $00,  $00,  $00,  $00,  $00,  $00,  $00,  $00,
    $00,  $00,  $00,  $00,  $00,  $00,  $00,  $00,  $00,
    $00,  $00,  $00,  $00,  $00,  $00,  $00,  $00,  $00,
    $00,  $00,  $00,  $00,  $00,  $00,  $00,  $00,  $00,
    $00,  $00,  $00,  $00,  $00,  $00,  $00,  $00,  $00,
    $00,  $00,  $00,  $00,  $00,  $00,  $00,  $00,  $00,
    $00,  $00,  $00,  $00,  $00,  $00,  $00,  $00,  $00,
    $00,  $00,  $00,  $00,  $00,  $00,  $00,  $00,  $00,
    $00,  $00,  $00,  $00,  $00,  $00,  $00,  $00,  $00,
    $00,  $00,  $00,  $00,  $00,  $00,  $00,  $00,  $00,
    $00,  $00,  $00,  $00,  $00,  $00,  $00,  $00,  $00,
    $00,  $00,  $00,  $00,  $00,  $00,  $00,  $00,  $00,
    $00,  $00,  $00,  $00,  $00,  $00,  $00,  $00,  $00,
    $00,  $00,  $00,  $00
  );

  mycore_string_hex_to_char_map : array [0 .. 16] of Byte = (
    $30,  $31,  $32,  $33,  $34,  $35,  $36,  $37,  $38,
    $39,  $41,  $42,  $43,  $44,  $45,  $46,  $00
  );

  mycore_string_char_to_two_hex_value : array [0 .. 256 ] of PChar = (
    '00', '01', '02', '03', '04', '05', '06', '07',
    '08', '09', '0A', 'OB', '0C', '0D', '0E', '0F',
    '10', '11', '12', '13', '14', '15', '16', '17',
    '18', '19', '1A', '1B', '1C', '1D', '1E', '1F',
    '20', '21', '22', '23', '24', '25', '26', '27',
    '28', '29', '2A', '2B', '2C', '2D', '2E', '2F',
    '30', '31', '32', '33', '34', '35', '36', '37',
    '38', '39', '3A', '3B', '3C', '3D', '3E', '3F',
    '40', '41', '42', '43', '44', '45', '46', '47',
    '48', '49', '4A', '4B', '4C', '4D', '4E', '4F',
    '50', '51', '52', '53', '54', '55', '56', '57',
    '58', '59', '5A', '5B', '5C', '5D', '5E', '5F',
    '60', '61', '62', '63', '64', '65', '66', '67',
    '68', '69', '6A', '6B', '6C', '6D', '6E', '6F',
    '70', '71', '72', '73', '74', '75', '76', '77',
    '78', '79', '7A', '7B', '7C', '7D', '7E', '7F',
    '80', '81', '82', '83', '84', '85', '86', '87',
    '88', '89', '8A', '8B', '8C', '8D', '8E', '8F',
    '90', '91', '92', '93', '94', '95', '96', '97',
    '98', '99', '9A', '9B', '9C', '9D', '9E', '9F',
    'A0', 'A1', 'A2', 'A3', 'A4', 'A5', 'A6', 'A7',
    'A8', 'A9', 'AA', 'AB', 'AC', 'AD', 'AE', 'AF',
    'B0', 'B1', 'B2', 'B3', 'B4', 'B5', 'B6', 'B7',
    'B8', 'B9', 'BA', 'BB', 'BC', 'BD', 'BE', 'BF',
    'C0', 'C1', 'C2', 'C3', 'C4', 'C5', 'C6', 'C7',
    'C8', 'C9', 'CA', 'CB', 'CC', 'CD', 'CE', 'CF',
    'D0', 'D1', 'D2', 'D3', 'D4', 'D5', 'D6', 'D7',
    'D8', 'D9', 'DA', 'DB', 'DC', 'DD', 'DE', 'DF',
    'E0', 'E1', 'E2', 'E3', 'E4', 'E5', 'E6', 'E7',
    'E8', 'E9', 'EA', 'EB', 'EC', 'ED', 'EE', 'EF',
    'F0', 'F1', 'F2', 'F3', 'F4', 'F5', 'F6', 'F7',
    'F8', 'F9', 'FA', 'FB', 'FC', 'FD', 'FE', 'FF',
    Pointer(0)
  );

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

(*mycore/mythread.h***************************************************************)

type
{$IFDEF MyCORE_BUILD_WITHOUT_THREADS}
  pmythread_t = ^mythread_t;
  mythread_t = record
    sys_last_error : Integer;
  end;
{$ELSE}
  ppmythread_t = ^pmythread_t;
  pmythread_t = ^mythread_t;
  mythread_t = record;

  pmythread_entry_t = ^mythread_entry_t;
  mythread_entry_t = record;

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
    mythread : mythread_t;
  end;

  pmythread_entry_t = ^mythread_entry_t;
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

    sys_last_error : Interger;

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
  pmthread_queue_node_t = ^mythread_queue_node_t;
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
procedure mythread_queue_list_destroy (queue_list : pmythread_queue_list);
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

  myhtml_tree_parse_flags_t = (
    MyHTML_TREE_PARSE_FLAGS_CLEAN                                       = $0000,
    MyHTML_TREE_PARSE_FLAGS_WITHOUT_BUILD_TREE                          = $0001,
    MyHTML_TREE_PARSE_FLAGS_WITHOUT_PROCESS_TOKEN                       = $0003,
    (* skip ws token, but not for RCDATA, RAWTEXT, CDATA and PLAINTEXT *)
    MyHTML_TREE_PARSE_FLAGS_SKIP_WHITESPACE_TOKEN                       = $0004,
    MyHTML_TREE_PARSE_FLAGS_WITHOUT_DOCTYPE_IN_TREE                     = $0008
  );

  (* token *)
  myhtml_token_type = (
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

  (* tags *)
  myhtml_tag_categories = (
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
  myhtml_tokenizer_state = (
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

  myhtml_insertion_mode = (
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

  myhtml_options = (
    MyHTML_OPTIONS_DEFAULT                                              = $0000,
    MyHTML_OPTIONS_PARSE_MODE_SINGLE                                    = $0001,
    MyHTML_OPTIONS_PARSE_MODE_ALL_IN_ONE                                = $0002,
    MyHTML_OPTIONS_PARSE_MODE_SEPARATELY                                = $0004
  );









implementation

end.

