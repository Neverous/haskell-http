module Mime where

get_content_type :: String -> String
get_content_type ".htm"     = "text/html"
get_content_type ".html"    = "text/html"
get_content_type ".xml"     = "text/xml"
get_content_type ".js"      = "application/javascript"
get_content_type ".json"    = "application/js"
get_content_type ".jpg"     = "image/jpeg"
get_content_type ".png"     = "image/png"
get_content_type ".ico"     = "image/x-icon"
get_content_type _          = "text/plain"
