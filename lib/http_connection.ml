type t = { ic : in_channel; oc : out_channel }

let make ~ic ~oc () = { ic; oc }

let input_request { ic; _ } =
  let rec input_lines ic =
    match In_channel.input_line ic with
    | None | Some "\r" -> []
    | Some line -> line :: input_lines ic
  in

  input_lines ic |> Http_request.of_lines

let output { oc; _ } code_status ?(headers = []) body =
  let buf = Buffer.create 14 in

  let response =
    let headers =
      Option.fold body ~none:headers ~some:(fun body ->
          ("Content-Length", string_of_int @@ String.length body) :: headers)
    in

    Http_response.{ code_status; headers; body }
  in

  Http_response.output_buffer buf response;
  Buffer.output_buffer oc buf

let output_html connection html_contents =
  output connection 200 (Some html_contents)
