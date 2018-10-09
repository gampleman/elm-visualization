/**
 * Welcome to your Workbox-powered service worker!
 *
 * You'll need to register this file in your web app and you should
 * disable HTTP caching for this file too.
 * See https://goo.gl/nhQhGp
 *
 * The rest of the code is auto-generated. Please don't update this file
 * directly; instead, make changes to your Workbox build configuration
 * and re-run your build process.
 * See https://goo.gl/2aRDsh
 */

importScripts("https://storage.googleapis.com/workbox-cdn/releases/3.6.2/workbox-sw.js");

/**
 * The workboxSW.precacheAndRoute() method efficiently caches and responds to
 * requests for URLs in the manifest.
 * See https://goo.gl/S9QRab
 */
self.__precacheManifest = [
  {
    "url": "Background/iframe.html",
    "revision": "faf5799ad367d53ac32d70d13a09db1f"
  },
  {
    "url": "Background/index.html",
    "revision": "1fc4ab1a0d9004298038c71e487a713c"
  },
  {
    "url": "BarChart/iframe.html",
    "revision": "b3873b4fbf548887b94f2ccfaaf8189b"
  },
  {
    "url": "BarChart/index.html",
    "revision": "fe31bbbbea21960210022363d64235c7"
  },
  {
    "url": "Centroid/iframe.html",
    "revision": "65432e2f7de7f705c222b1d6a08786bf"
  },
  {
    "url": "Centroid/index.html",
    "revision": "6b13d154d95c0f03dadf51d0d58475c8"
  },
  {
    "url": "CornerRadius/iframe.html",
    "revision": "bea59d7dea04a2794ce333838946bce4"
  },
  {
    "url": "CornerRadius/index.html",
    "revision": "f4581b8a261a6e38e37f934aaa0344f4"
  },
  {
    "url": "CrimeViz/iframe.html",
    "revision": "fef997785f31ee17f82c9732fc748255"
  },
  {
    "url": "CrimeViz/index.html",
    "revision": "646fae0d95d59b310f7058c7692bcced"
  },
  {
    "url": "Cross/iframe.html",
    "revision": "ce5694c0bedbe2f7bf42c73d9a496176"
  },
  {
    "url": "Cross/index.html",
    "revision": "7e7760da0519cad1ea81216f8baf5031"
  },
  {
    "url": "Curves/iframe.html",
    "revision": "fe35c7a923f9e5eb5866ebe99ab31db0"
  },
  {
    "url": "Curves/index.html",
    "revision": "cee5a115852f93577226bdbdda438229"
  },
  {
    "url": "CustomPieChart/iframe.html",
    "revision": "fe1db3eb003de7046405d791d042abb5"
  },
  {
    "url": "CustomPieChart/index.html",
    "revision": "3b561a9779edb305375e1c26af7577d7"
  },
  {
    "url": "ForceDirectedGraph/iframe.html",
    "revision": "462354660d33199918e8d2731f179e2a"
  },
  {
    "url": "ForceDirectedGraph/index.html",
    "revision": "327cee9479f5ce2f7688f6ab480f2873"
  },
  {
    "url": "Histogram/iframe.html",
    "revision": "60d6577972edf4a371e9c1f837e96277"
  },
  {
    "url": "Histogram/index.html",
    "revision": "dd63d4e0e0aab5d9f43ebbcc27900d3f"
  },
  {
    "url": "HistogramChart/iframe.html",
    "revision": "1121278e2e81229cb72d033271b7df1e"
  },
  {
    "url": "HistogramChart/index.html",
    "revision": "5dcb4534ed5782e443ddb54d309dcdca"
  },
  {
    "url": "index.html",
    "revision": "f56fdd59d0569fb0626dc3a646f4dec2"
  },
  {
    "url": "LineChart/iframe.html",
    "revision": "a45cb96a392a306324c8e237c2b798d1"
  },
  {
    "url": "LineChart/index.html",
    "revision": "8e640985cf45e138b7196dff0c076a30"
  },
  {
    "url": "NorwegianCarSales/iframe.html",
    "revision": "4433fb3cee8dcda6e5d863dd5dad9165"
  },
  {
    "url": "NorwegianCarSales/index.html",
    "revision": "c55dcf6a5cc9a55b77d0d5f71de3bd21"
  },
  {
    "url": "PadAngle/iframe.html",
    "revision": "7b6da1f7de6c0a7673c8a4d30d54e54d"
  },
  {
    "url": "PadAngle/index.html",
    "revision": "c863b68b7c7edd14db39b0826a7b6fb8"
  },
  {
    "url": "Petals/iframe.html",
    "revision": "a8851d3bf54adf291f51d78704e684c9"
  },
  {
    "url": "Petals/index.html",
    "revision": "1fa164ed23f5ddbc62d3d2172f84c5b6"
  },
  {
    "url": "PieChart/iframe.html",
    "revision": "7ebcb77aa04651bf0e9d084d77d910aa"
  },
  {
    "url": "PieChart/index.html",
    "revision": "e7e16ba33b21c1ad20876015627fa56b"
  },
  {
    "url": "PolarPlot/iframe.html",
    "revision": "2eb2eecd3489a8663baaad6d8a78bae9"
  },
  {
    "url": "PolarPlot/index.html",
    "revision": "e92616d56ae946bbd2dbbb25d292b7d5"
  },
  {
    "url": "PopulationMinnesota/iframe.html",
    "revision": "812b0c83ba10942f11486e3ba8d2fa7f"
  },
  {
    "url": "PopulationMinnesota/index.html",
    "revision": "eb1d409a8c4de0002bb91c920bdc96b9"
  },
  {
    "url": "StackedBarChart/iframe.html",
    "revision": "f84df204eef17e32b767c7e4c738a87a"
  },
  {
    "url": "StackedBarChart/index.html",
    "revision": "bc7598e3277944b76e9b66e6852d89eb"
  },
  {
    "url": "Background/preview.png",
    "revision": "5c2488ef304cfd1836a50fa0597413d9"
  },
  {
    "url": "BarChart/preview.png",
    "revision": "c8bbe7936f7d7652288e369492c85a64"
  },
  {
    "url": "Centroid/preview.png",
    "revision": "3fb618c0a60dbc882174d5651005b982"
  },
  {
    "url": "CornerRadius/preview.png",
    "revision": "c929cce64e97aca7ef4f84ea54fb34cf"
  },
  {
    "url": "CrimeViz/preview.png",
    "revision": "94e53040782992d4e162bf1158d3ae80"
  },
  {
    "url": "Cross/preview.png",
    "revision": "c696bfe81d850217f1ae1dc9ab9ad549"
  },
  {
    "url": "Curves/preview.png",
    "revision": "d53fc06ba126038618f411420857fac3"
  },
  {
    "url": "CustomPieChart/preview.png",
    "revision": "da46d7dc2d9282e56493b755b252fa94"
  },
  {
    "url": "ForceDirectedGraph/preview.png",
    "revision": "4243f98206fd6f1fffef751044452b8f"
  },
  {
    "url": "Histogram/preview.png",
    "revision": "50ac7bddddc6d997959cc9bf79dae251"
  },
  {
    "url": "HistogramChart/preview.png",
    "revision": "48e5d62c9f99f4fc12119fbdea38b9cc"
  },
  {
    "url": "LineChart/preview.png",
    "revision": "b2a54dfdc16164a7782c6b91e1a47bb9"
  },
  {
    "url": "NorwegianCarSales/preview.png",
    "revision": "433a733999ef9f722069aa3e2a4cd402"
  },
  {
    "url": "PadAngle/preview.png",
    "revision": "805c106871c79365e3fca7e0c6ffc27d"
  },
  {
    "url": "Petals/preview.png",
    "revision": "4c1f1656b9a27a06cdb105fdb4aebf76"
  },
  {
    "url": "PieChart/preview.png",
    "revision": "04cbc28271421a44df82899ef4978969"
  },
  {
    "url": "PolarPlot/preview.png",
    "revision": "4361ac520753fbfc141004c41024647d"
  },
  {
    "url": "PopulationMinnesota/preview.png",
    "revision": "ee7085db94bb39c75252318f8ec9b267"
  },
  {
    "url": "StackedBarChart/preview.png",
    "revision": "da468815a5c894c5c5902b43c8ffe6b9"
  }
].concat(self.__precacheManifest || []);
workbox.precaching.suppressWarnings();
workbox.precaching.precacheAndRoute(self.__precacheManifest, {});
