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

  myhtml_insetion_mode = (
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

  pmycore_string_t = ^mycore_string_t;
  mycore_string_t = record
  end;

  (* incoming buffer *)
  mycore_incoming_buffer_t = record
  end;

  (* char references *)
  pmyhtml_data_process_entry_t = ^myhtml_data_process_entry_t;
  myhtml_data_process_entry_t = record
  end;

  myhtml_tree_temp_tag_name_t = record
  end;

  myhtml_tree_insertion_list_t = record
  end;

  myhtml_tree_token_list_t = record
  end;

  myhtml_tree_list_t = record
  end;

  myhtml_tree_doctype_t = record
  end;

  myhtml_async_args_t = record
  end;

  pmyhtml_tree_node_t = ^myhtml_tree_node_t;
  myhtml_tree_node_t = record
  end;

  pmyhtml_tree_t = ^myhtml_tree_t;
  myhtml_tree_t = record
  end;

  pmyhtml_token_index_t = ^myhtml_token_index_t;
  myhtml_token_index_t = type QWord;
  pmyhtml_token_attr_index_t = ^myhtml_token_attr_index_t;
  myhtml_token_attr_index_t = type QWord;

  pmyhtml_token_replacement_entry_t = ^myhtml_token_replacement_entry_t;
  myhtml_token_replacement_entry_t = record
  end;

  pmyhtml_token_namespace_replacement_t = ^myhtml_token_namespace_replacement_t;
  myhtml_token_namespace_replacement_t = record
  end;

  pmyhtml_token_attr_t = ^myhtml_token_attr_t;
  myhtml_token_attr_t = record
  end;

  pmyhtml_token_node_t = ^myhtml_token_node_t;
  myhtml_token_node_t = record
  end;

  pmyhtml_token_t = ^myhtml_token_t;
  myhtml_token_t = record
  end;

  pmyhtml_tag_index_node_t = ^myhtml_tag_index_node_t;
  myhtml_tag_index_node_t = record
  end;

  pmyhtml_tag_index_entry_t = ^myhtml_tag_index_entry_t;
  myhtml_tag_index_entry_t = record
  end;

  pmyhtml_tag_index_t = ^myhtml_tag_index_t;
  myhtml_tag_index_t = record
  end;

  pmyhtml_tag_id_t = ^myhtml_tag_id_t;
  myhtml_tag_id_t = type QWord;

  pmyhtml_tag_t = ^myhtml_tag_t;
  myhtml_tag_t = record
  end;

  (* stream *)
  pmyhtml_stream_buffer_entry_t = ^myhtml_stream_buffer_entry_t;
  myhtml_stream_buffer_entry_t = record
  end;

  pmyhtml_stream_buffer_t = ^myhtml_stream_buffer_t;
  myhtml_stream_buffer_t = record
  end;

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

  pmyhtml_tree_attr_t = ^myhtml_tree_attr_t;
  myhtml_tree_attr_t = type myhtml_token_attr_t;

  pmyhtml_collection_t = ^myhtml_collection_t;
  myhtml_collection_t = record
  end;

  (* callbacks *)
  pmycore_callback_serialize = ^mycore_callback_serialize;
  mycore_callback_serialize = function (const buffer : PChar; size : QWord;
    ctx : Pointer) : mystatus_t of object;

  pmyhtml_callback_token_f = ^myhtml_callback_token_f;
  myhtml_callback_token_f = function (tree : pmyhtml_tree_t; token :
    pmyhtml_token_node_t; ctx : Pointer) : Pointer of object;

  pmyhtml_callback_tree_node_f = ^myhtml_callback_tree_node_f;
  myhtml_callback_tree_node_f = procedure (tree : pmyhtml_tree_t; node :
    pmyhtml_tree_node_t; ctx : Pointer) of object;

  (* parser state function *)
  pmyhtml_tokenizer_state_f = ^myhtml_tokenizer_state_f;
  myhtml_tokenizer_state_f = function (tree : pmyhtml_tree_t; token_node :
    pmyhtml_token_node_t; const html : PChar; html_offset : QWord;
    html_size : QWord) : QWord of object;

  (* parser insertion mode function *)
  pmyhtml_insertion_f = ^myhtml_insertion_f;
  myhtml_insertion_f = function (tree : pmyhtml_tree_t;
    token : pmyhtml_token_node_t) : Boolean of object;

  (* char references state *)
  pmyhtml_data_process_state_f = ^myhtml_data_process_state_f;
  myhtml_data_process_state_f = function (charef: pmyhtml_data_process_entry_t;
    str : pmycore_string_t; const data : PChar; offset : QWord; size : QWord)
    : QWord of object;

  (* find attribute value functions *)
  pmyhtml_attribute_value_find_f = ^myhtml_attribute_value_find_f;
  myhtml_attribute_value_find_f = function (str_key : pmycore_string_t;
    const value : PChar; value_len : QWord) : Boolean of object;

  pmyhtml_t = ^myhtml_t;
  myhtml_t = record
    thread_stream : pmythread_t;
    thread_batch : pmythread_t;
    thread_list : array [0 .. 2] of pmythread_t;
    thread_total : QWord;

    parse_state_func : pmyhtml_tokenizer_state_f;
    insertion_func : pmyhtml_insertion_f;

    opt : myhtml_options;
    marker : pmyhtml_tree_node_t;
  end;

{$IFDEF WINDOWS}
  const MyHTMLLib = 'libmyhtml.dll';
{$ENDIF}
{$IFDEF LINUX}
  const MyHTMLLib = 'libmyhtml.so';
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

