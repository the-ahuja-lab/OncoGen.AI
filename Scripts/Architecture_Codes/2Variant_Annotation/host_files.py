# Save this as `simple_server.py`

import http.server
import socketserver

PORT = 1235

class Handler(http.server.SimpleHTTPRequestHandler):
    pass

with socketserver.TCPServer(("", PORT), Handler) as httpd:
    print(f"Serving files at http://localhost:{PORT}")
    httpd.serve_forever()

