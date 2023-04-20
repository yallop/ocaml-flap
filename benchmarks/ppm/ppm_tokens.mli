(*
 * Copyright (c) 2023 Jeremy Yallop
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

include Asp.Types.TOKEN
        with type t = Ppm_tokens_base.t
        with type utag = Ppm_tokens_base.utag
        with type 'a tag = 'a Ppm_tokens_base.tag
