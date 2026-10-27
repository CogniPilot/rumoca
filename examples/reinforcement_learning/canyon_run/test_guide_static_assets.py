"""Browser asset custody and traversal controls; no native process is launched."""
from http.client import HTTPConnection
from pathlib import Path
from tempfile import TemporaryDirectory
import threading
import unittest
from unittest.mock import patch
from http.server import ThreadingHTTPServer
import serve


class StaticAssets(unittest.TestCase):
    def request(self, path, method="GET"):
        client = HTTPConnection('127.0.0.1', self.server.server_port)
        try:
            client.request(method, path)
            response = client.getresponse()
            return response.status, dict(response.getheaders()), response.read()
        finally:
            client.close()

    def setUp(self):
        self.directory = TemporaryDirectory(prefix='rumoca-guide-assets-')
        self.root = Path(self.directory.name)
        self.pkg, self.vendor = self.root/'pkg', self.root/'vendor'
        self.pkg.mkdir()
        self.vendor.mkdir()
        (self.pkg/'test.wasm').write_bytes(b'fixture wasm')
        (self.vendor/'editor.js').write_text('fixture editor')
        (self.root/'outside.txt').write_text('must not be served')
        self.roots = patch.multiple(serve, create=True, WASM=self.pkg, VENDOR=self.vendor)
        self.roots.start()
        self.server = ThreadingHTTPServer(('127.0.0.1', 0), serve.Handler)
        self.thread = threading.Thread(target=self.server.serve_forever)
        self.thread.start()

    def tearDown(self):
        self.server.shutdown()
        self.server.server_close()
        self.thread.join()
        self.roots.stop()
        self.directory.cleanup()

    def test_wasm_has_browser_mime_and_exact_bytes(self):
        status, headers, body = self.request('/pkg/test.wasm')
        self.assertEqual((status, body), (200, b'fixture wasm'))
        self.assertEqual(headers['Content-Type'], 'application/wasm')
        self.assertEqual(headers['Cache-Control'], 'no-store')

    def test_head_supports_package_discovery_without_a_body(self):
        status, headers, body = self.request('/pkg/test.wasm', 'HEAD')
        self.assertEqual((status, body), (200, b''))
        self.assertEqual(int(headers['Content-Length']), len(b'fixture wasm'))
        self.assertEqual(headers['Content-Type'], 'application/wasm')

    def test_vendor_has_exact_bytes(self):
        status, _, body = self.request('/vendor/editor.js')
        self.assertEqual((status, body), (200, b'fixture editor'))

    def test_traversal_and_symlink_escape_refuse(self):
        (self.pkg/'escape.txt').symlink_to(self.root/'outside.txt')
        for path in ['/pkg/../outside.txt', '/vendor/../outside.txt', '/pkg/escape.txt']:
            status, _, body = self.request(path)
            self.assertEqual(status, 404)
            self.assertNotIn(b'must not be served', body)


if __name__ == '__main__':
    unittest.main()
