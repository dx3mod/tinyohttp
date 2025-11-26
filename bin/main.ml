open Toyhttpserver

let handle_incoming_http_request connection request =
  let html_page =
    let open Tyxml.Html in
    html
      (head (title @@ txt "my cringe page") [])
      (body
         [
           header
             [
               h1
                 [
                   txt "Bye!";
                   space ();
                   entity "mdash";
                   space ();
                   txt request.Http_request.path;
                 ];
             ];
           ul
           @@ List.map
                (fun (key, value) ->
                  li
                    [
                      b [ txt key; space () ];
                      entity "mdash";
                      txt value;
                      space ();
                    ])
                request.headers;
         ])
  in

  Format.asprintf "%a" (Tyxml.Html.pp ()) html_page
  |> Http_connection.output_html connection

let () =
  Http_server.with_serve
    (~host:"127.0.0.1", ~port:8080)
    handle_incoming_http_request
