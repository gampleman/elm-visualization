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
    "revision": "aa07bb88332b9fd12b22706fd5c5d155"
  },
  {
    "url": "BarChart/iframe.html",
    "revision": "b3873b4fbf548887b94f2ccfaaf8189b"
  },
  {
    "url": "BarChart/index.html",
    "revision": "3f5b47eeb5ebd25a179669858944f354"
  },
  {
    "url": "Centroid/iframe.html",
    "revision": "65432e2f7de7f705c222b1d6a08786bf"
  },
  {
    "url": "Centroid/index.html",
    "revision": "3b8928ff8186e2216f7741d873dc3ed3"
  },
  {
    "url": "CornerRadius/iframe.html",
    "revision": "bea59d7dea04a2794ce333838946bce4"
  },
  {
    "url": "CornerRadius/index.html",
    "revision": "5031b64d5bbe12d3b464c832a236c5cc"
  },
  {
    "url": "CrimeViz/iframe.html",
    "revision": "fef997785f31ee17f82c9732fc748255"
  },
  {
    "url": "CrimeViz/index.html",
    "revision": "67682d91da4b2fe8f59c7dd9da9652ea"
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
    "revision": "3c4d45b0fca7d438d18c4095cc69780f"
  },
  {
    "url": "CustomPieChart/iframe.html",
    "revision": "7cf5d125d0be04898170fa47f59cc76b"
  },
  {
    "url": "CustomPieChart/index.html",
    "revision": "8fc446191b1539f5d214d52fa1463b66"
  },
  {
    "url": "ForceDirectedGraph/iframe.html",
    "revision": "462354660d33199918e8d2731f179e2a"
  },
  {
    "url": "ForceDirectedGraph/index.html",
    "revision": "c3dfb2ef5eb3156dbcd9722272874f09"
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
    "revision": "725b944fd5270a1a0181870c9c818a5e"
  },
  {
    "url": "index.html",
    "revision": "b5dad7ddb6916ca2aa1d642e692882d5"
  },
  {
    "url": "LineChart/iframe.html",
    "revision": "a45cb96a392a306324c8e237c2b798d1"
  },
  {
    "url": "LineChart/index.html",
    "revision": "f9dfa4a8554ed098abaceadddf55ce88"
  },
  {
    "url": "NorwegianCarSales/iframe.html",
    "revision": "4433fb3cee8dcda6e5d863dd5dad9165"
  },
  {
    "url": "NorwegianCarSales/index.html",
    "revision": "2c05f8829cbef9a695ae8e4a08f98563"
  },
  {
    "url": "PadAngle/iframe.html",
    "revision": "7b6da1f7de6c0a7673c8a4d30d54e54d"
  },
  {
    "url": "PadAngle/index.html",
    "revision": "0db29adf167390ae6a6be2f802516fa7"
  },
  {
    "url": "Petals/iframe.html",
    "revision": "a8851d3bf54adf291f51d78704e684c9"
  },
  {
    "url": "Petals/index.html",
    "revision": "8ca4f190781ed6e14b8abe3381f742dd"
  },
  {
    "url": "PieChart/iframe.html",
    "revision": "7ebcb77aa04651bf0e9d084d77d910aa"
  },
  {
    "url": "PieChart/index.html",
    "revision": "8c37bae09d9ff62c9c3153a0bbca0971"
  },
  {
    "url": "PolarPlot/iframe.html",
    "revision": "2eb2eecd3489a8663baaad6d8a78bae9"
  },
  {
    "url": "PolarPlot/index.html",
    "revision": "54f46df0b963c3b6b0455823ad05d3a2"
  },
  {
    "url": "PopulationMinnesota/iframe.html",
    "revision": "812b0c83ba10942f11486e3ba8d2fa7f"
  },
  {
    "url": "PopulationMinnesota/index.html",
    "revision": "1192551f53e8ce18da5cf38b46bddb74"
  },
  {
    "url": "StackedBarChart/iframe.html",
    "revision": "f84df204eef17e32b767c7e4c738a87a"
  },
  {
    "url": "StackedBarChart/index.html",
    "revision": "226fd6df52dff4ab93f0331e44bfe497"
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
    "revision": "93ba163cca30d4c718888330dcfb8323"
  },
  {
    "url": "ForceDirectedGraph/preview.png",
    "revision": "667ff83f94d5cb2a1f0f921c425daeaf"
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
