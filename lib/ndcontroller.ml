(*
 *   Copyright (C) Eincalc 2025. All Rights Reserved.
 *   For internal use only. Do not redistribute.
 *
 *   Copyright laws and international treaties protect this app. Unauthorized 
 *   redistribution of this app without express, written permission from our legal 
 *   department may entail severe civil or criminal penalties.
 *
 *)

(* 
 * - Track updated cell locations (track write calls and adds along with shape sizes)
 * - Run event callbacks e.g. at parsing to suggest stuff
 * - Notify view of errors
 * - manage execution instances
 * - maintain history of executed trees
 * - reuse tensors on re-execute once change subscribers are implemented
 *)
