type t = {
  meth : [ `POST | `GET ];
  path : string;
  headers : (string * string) list;
}

let parse_first_line line =
  match String.split_on_char ' ' @@ String.trim line with
  | [ "GET"; path; "HTTP/1.1" ] -> (`GET, path)
  | [ "POST"; path; "HTTP/1.1" ] -> (`POST, path)
  | _ -> failwith "failed parse first line"

let parse_header_line line =
  match String.split_on_char ':' line with
  | [ key; value ] -> Some (key, value)
  | _ -> None

let of_lines = function
  | first_line :: headers ->
      let meth, path = parse_first_line first_line
      and headers = List.filter_map parse_header_line headers in

      { meth; path; headers }
  | _ -> failwith "failed to parse HTTP request"
