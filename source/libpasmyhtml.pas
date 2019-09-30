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

type
  mycore_status_t = (
    MyCORE_STATUS_OK                                         = $0000,
    MyCORE_STATUS_ERROR                                      = $0001,
    MyCORE_STATUS_ERROR_MEMORY_ALLOCATION                    = $0002,
    MyCORE_STATUS_THREAD_ERROR_MEMORY_ALLOCATION             = $0009,
    MyCORE_STATUS_THREAD_ERROR_LIST_INIT                     = $000a,
    MyCORE_STATUS_THREAD_ERROR_ATTR_MALLOC                   = $000b,
    MyCORE_STATUS_THREAD_ERROR_ATTR_INIT                     = $000c,
    MyCORE_STATUS_THREAD_ERROR_ATTR_SET                      = $000d,
    MyCORE_STATUS_THREAD_ERROR_ATTR_DESTROY                  = $000e,
    MyCORE_STATUS_THREAD_ERROR_NO_SLOTS                      = $000f,
    MyCORE_STATUS_THREAD_ERROR_BATCH_INIT                    = $0010,
    MyCORE_STATUS_THREAD_ERROR_WORKER_MALLOC                 = $0011,
    MyCORE_STATUS_THREAD_ERROR_WORKER_SEM_CREATE             = $0012,
    MyCORE_STATUS_THREAD_ERROR_WORKER_THREAD_CREATE          = $0013,
    MyCORE_STATUS_THREAD_ERROR_MASTER_THREAD_CREATE          = $0014,
    MyCORE_STATUS_THREAD_ERROR_SEM_PREFIX_MALLOC             = $0032,
    MyCORE_STATUS_THREAD_ERROR_SEM_CREATE                    = $0033,
    MyCORE_STATUS_THREAD_ERROR_QUEUE_MALLOC                  = $003c,
    MyCORE_STATUS_THREAD_ERROR_QUEUE_NODES_MALLOC            = $003d,
    MyCORE_STATUS_THREAD_ERROR_QUEUE_NODE_MALLOC             = $003e,
    MyCORE_STATUS_THREAD_ERROR_MUTEX_MALLOC                  = $0046,
    MyCORE_STATUS_THREAD_ERROR_MUTEX_INIT                    = $0047,
    MyCORE_STATUS_THREAD_ERROR_MUTEX_LOCK                    = $0048,
    MyCORE_STATUS_THREAD_ERROR_MUTEX_UNLOCK                  = $0049,
    MyCORE_STATUS_PERF_ERROR_COMPILED_WITHOUT_PERF           = $0050,
    MyCORE_STATUS_PERF_ERROR_FIND_CPU_CLOCK                  = $0051,
    MyCORE_STATUS_MCOBJECT_ERROR_CACHE_CREATE                = $0055,
    MyCORE_STATUS_MCOBJECT_ERROR_CHUNK_CREATE                = $0056,
    MyCORE_STATUS_MCOBJECT_ERROR_CHUNK_INIT                  = $0057,
    MyCORE_STATUS_MCOBJECT_ERROR_CACHE_REALLOC               = $0058,
    MyCORE_STATUS_ASYNC_ERROR_LOCK                           = $0060,
    MyCORE_STATUS_ASYNC_ERROR_UNLOCK                         = $0061,
    MyCORE_STATUS_ERROR_NO_FREE_SLOT                         = $0062
  );

  mystatus_t = type Cardinal;

  (* thread *)
  mythread_queua_list_entry_t = record
  end;

  mythread_queue_thread_param_t = record
  end;

  mythread_queue_list_t = record
  end;

  mythread_queue_node_t = record
  end;

  mythread_queue_t = record
  end;

  mythread_id_t = type QWord;

  mythread_context_t = record
  end;

  mythread_entry_t = record
  end;

  pmythread_t = ^mythred_t;
  mythread_t = record
  end;

  (* mystring *)
  mycore_string_raw_t = record
  end;

  mycore_string_t = record
  end;

  (* incoming buffer *)
  mycore_incoming_buffer_t = record
  end;

  (* callbacks *)
  mycore_callback_serialize = function (const buffer : PChar; size : QWord;
    ctx : Pointer) : mystatus_t of object;

  myhtml = record
    thread_stream : pmythread_t;
    thread_batch : pmythread_t;
    thread_list : array [0 .. 2] of pmythread_t;
    thread_total : QWord;

    //parse_state_func : pmyhtml_tokenizer_state_f;
    //insertion_func : pmyhtml_insertion_f;

    //opt : myhtml_options;
    //marker : pmyhtml_tree_node_t;
  end;

{$IFDEF WINDOWS}
  MyHTMLLib = 'libmyhtml.dll';
{$ENDIF}
{$IFDEF LINUX}
  MyHTMLLib = 'libmyhtml.so';
{$ENDIF}

  function mycore_malloc (size : QWord) : Pointer; cdecl; external MyHTMLLib;
  function mycore_realloc (dst : Pointer; size : QWord) : Pointer; cdecl;
    external MyHTMLLib;
  function mycore_calloc (num : QWord; size : QWord) : Pointer; cdecl;
    external MyHTMLLib;
  function mycode_free (dst : Pointer) : Pointer; cdecl; external MyHTMLLib;

  (* io *)
  function mycore_fopen (const filename : PChar; const mode : PChar)
    : Pointer; cdecl; external MyHTMLLib;
  function mycore_fclose (stream : Pointer) : Integer; cdecl;
    external MyHTMLLib;
  function mycore_fread (buffer : Pointer; size : QWord; count : QWord;
    stream : Pointer) : QWord; cdecl; external MyHTMLLib;
  function mycore_fwrite (const buffer : Pointer; size : QWord; count : QWord;
    stream : Pointer) : QWord; cdecl; external MyHTMLLib;
  function mycore_fflush (stream : Pointer) : Integer; cdecl;
    external MyHTMLLib;
  function mycore_fseek (stream : Pointer; offset : Longint; origin : Integer)
    : Integer; cdecl; external MyHTMLLib;
  function mycore_ftell (stream : Pointer) : Longint; cdecl;
    external MyHTMLLib;
  function mycore_ferror (stream : Pointer) : Integer; cdecl;
    external MyHTMLLib;
  procedure mycore_setbuf (stream : Pointer; buffer : PChar); cdecl;
    external MyHTMLLib;

implementation

end.

