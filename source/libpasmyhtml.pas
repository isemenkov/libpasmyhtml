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
    MyCORE_STATUS_OK                                         = $0000;
    MyCORE_STATUS_ERROR                                      = $0001;
    MyCORE_STATUS_ERROR_MEMORY_ALLOCATION                    = $0002;
    MyCORE_STATUS_THREAD_ERROR_MEMORY_ALLOCATION             = $0009;
    MyCORE_STATUS_THREAD_ERROR_LIST_INIT                     = $000a;
    MyCORE_STATUS_THREAD_ERROR_ATTR_MALLOC                   = $000b;
    MyCORE_STATUS_THREAD_ERROR_ATTR_INIT                     = $000c;
    MyCORE_STATUS_THREAD_ERROR_ATTR_SET                      = $000d;
    MyCORE_STATUS_THREAD_ERROR_ATTR_DESTROY                  = $000e;
    MyCORE_STATUS_THREAD_ERROR_NO_SLOTS                      = $000f;
    MyCORE_STATUS_THREAD_ERROR_BATCH_INIT                    = $0010;
    MyCORE_STATUS_THREAD_ERROR_WORKER_MALLOC                 = $0011;
    MyCORE_STATUS_THREAD_ERROR_WORKER_SEM_CREATE             = $0012;
    MyCORE_STATUS_THREAD_ERROR_WORKER_THREAD_CREATE          = $0013;
    MyCORE_STATUS_THREAD_ERROR_MASTER_THREAD_CREATE          = $0014;
    MyCORE_STATUS_THREAD_ERROR_SEM_PREFIX_MALLOC             = $0032;
    MyCORE_STATUS_THREAD_ERROR_SEM_CREATE                    = $0033;
    MyCORE_STATUS_THREAD_ERROR_QUEUE_MALLOC                  = $003c;
    MyCORE_STATUS_THREAD_ERROR_QUEUE_NODES_MALLOC            = $003d;
    MyCORE_STATUS_THREAD_ERROR_QUEUE_NODE_MALLOC             = $003e;
    MyCORE_STATUS_THREAD_ERROR_MUTEX_MALLOC                  = $0046;
    MyCORE_STATUS_THREAD_ERROR_MUTEX_INIT                    = $0047;
    MyCORE_STATUS_THREAD_ERROR_MUTEX_LOCK                    = $0048;
    MyCORE_STATUS_THREAD_ERROR_MUTEX_UNLOCK                  = $0049;
    MyCORE_STATUS_PERF_ERROR_COMPILED_WITHOUT_PERF           = $0050;
    MyCORE_STATUS_PERF_ERROR_FIND_CPU_CLOCK                  = $0051;
    MyCORE_STATUS_MCOBJECT_ERROR_CACHE_CREATE                = $0055;
    MyCORE_STATUS_MCOBJECT_ERROR_CHUNK_CREATE                = $0056;
    MyCORE_STATUS_MCOBJECT_ERROR_CHUNK_INIT                  = $0057;
    MyCORE_STATUS_MCOBJECT_ERROR_CACHE_REALLOC               = $0058;
    MyCORE_STATUS_ASYNC_ERROR_LOCK                           = $0060;
    MyCORE_STATUS_ASYNC_ERROR_UNLOCK                         = $0061;
    MyCORE_STATUS_ERROR_NO_FREE_SLOT                         = $0062;
  );




implementation

end.

