open Toyhttpserver

let () =
  Http_connection.with_http_serve @@ fun connection request ->
  Http_connection.output_html connection
  @@ Printf.sprintf "<h1>Bye! %s</h1><ul>%s</ul>" request.path
  @@ String.concat ""
  @@ List.map
       (fun (k, v) -> Printf.sprintf "<li><b>%s</b> &mdash; %s</li>" k v)
       request.headers
