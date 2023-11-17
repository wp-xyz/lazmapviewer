{ Include file for map provider registration.
  Called from mvEngine.pas. }

{ Developer links:

- https://gis-lab.info/forum/viewtopic.php?f=19&t=19763
- https://a.tile.openstreetmap.org/16/51693/32520.png
- https://vec01.maps.yandex.net/tiles?l=map&x=51693+570&y=32520&z=16&scale=1&lang=ru_RU
- https://www.linux.org.ru/forum/development/9038716
- https://wiki.openstreetmap.org/wiki/Tiles
- https://pubs.usgs.gov/pp/1395/report.pdf
- https://wiki.openstreetmap.org/wiki/Slippy_map_tilenames#Tile_numbers_to_lon..2Flat.
- https://mc.bbbike.org/mc/?num=2
- https://mc.bbbike.org/mc/?lon=37.62178&lat=55.740937&zoom=14&num=1&mt0=opentopomap&mt1=mapnik-german
- https://t.ssl.ak.dynamic.tiles.virtualearth.net/comp/ch/12031010103311?mkt=ru-RU&it=G,BX,RL&shading=hill&n=z&og=677&c4w=1&cstl=vb&src=h
- https://www.thunderforest.com/docs/map-tiles-api/

Some providers submitted by

- "kangozidev": https://github.com/wp-xyz/lazmapviewer/issues/1#issuecomment-1585534499
- "regs01":     https://gitlab.com/freepascal.org/lazarus/ccr/-/issues/39052
}

  // OpenStreetMap section
  MapWin.MapProvider := AddMapProvider('OpenStreetMap Mapnik', ptEPSG3857, 'http://%serv%.tile.openstreetmap.org/%z%/%x%/%y%.png', 0, 19, 3, @GetSvrLetter);
  // OSM Wikipedia no longer available for services outside Wikimedia projects.
  //   AddMapProvider('OpenStreetMap Wikipedia', ptEPSG3857, 'https://maps.wikimedia.org/osm-intl/%z%/%x%/%y%.png', 0, 19, 3, @GetSvrLetter);
  // OSM Sputnik is no longer available. Keeping it here for reference only...
  //  AddMapProvider('OpenStreetMap Sputnik', ptEPSG3857, 'https://%serv%.tilessputnik.ru/tiles/kmt2/%z%/%x%/%y%.png', 0, 19, 3, @GetSvrLetter);
  AddMapProvider('OpenStreetMap.fr Hot', ptEPSG3857, 'https://%serv%.tile.openstreetmap.fr/hot/%z%/%x%/%y%.png', 0, 18, 3, @GetSvrLetter);
  AddMapProvider('Open Topo Map', ptEPSG3857, 'http://%serv%.tile.opentopomap.org/%z%/%x%/%y%.png', 0, 19, 3, @GetSvrLetter);
  AddMapProvider('OpenStreetMap.fr Cycle Map', ptEPSG3857, 'https://dev.%serv%.tile.openstreetmap.fr/cyclosm/%z%/%x%/%y%.png', 0, 18, 3, @GetSvrLetter);
  AddMapProvider('OSM Refuges', ptEPSG3857, 'https://maps.refuges.info/hiking/%z%/%x%/%y%.png', 0, 19, 4, nil);

  // API Key required
  if (ThunderForest_ApiKey <> '') then
  begin
    // Registration required to access the ThunderForst maps:
    //    https://www.thunderforest.com/docs/apikeys/
    // The API key is found on their website after registration and logging in.
    // Store the API key in the ini file under key [ThunderForest] as item API_Key
    AddMapProvider('ThunderForest Open Cycle Map', ptEPSG3857, 'https://tile.thunderforest.com/cycle/%z%/%x%/%y%.png?apikey=' + ThunderForest_ApiKey, 0, 19, 3, nil, nil, nil, nil);
    AddMapProvider('ThunderForest OpenStreetMap Transport', ptEPSG3857, 'https://tile.thunderforest.com/transport/%z%/%x%/%y%.png?apikey=' + ThunderForest_ApiKey, 0, 19, 3, nil, nil, nil, nil);

    AddMapProvider('ThunderForest Neighbourhood', ptEPSG3857, 'https://tile.thunderforest.com/neighbourhood/%z%/%x%/%y%.png?apikey=' + ThunderForest_ApiKey, 0, 19, 4, nil);
    AddMapProvider('ThunderForest Atlas', ptEPSG3857, 'https://tile.thunderforest.com/atlas/%z%/%x%/%y%.png?apikey=' + ThunderForest_ApiKey, 0, 19, 4, nil);
    AddMapProvider('ThunderForest Pioneer', ptEPSG3857, 'https://tile.thunderforest.com/pioneer/%z%/%x%/%y%.png?apikey=' + ThunderForest_ApiKey, 0, 19, 4, nil);
    AddMapProvider('ThunderForest Outdoors', ptEPSG3857, 'https://tile.thunderforest.com/outdoors/%z%/%x%/%y%.png?apikey=' + ThunderForest_ApiKey, 0, 19, 4, nil);
    AddMapProvider('ThunderForest Landscape', ptEPSG3857, 'https://tile.thunderforest.com/outdoors/%z%/%x%/%y%.png?apikey=' + ThunderForest_ApiKey, 0, 19, 4, nil);
    AddMapProvider('ThunderForest Mobile-Atlas', ptEPSG3857, 'https://tile.thunderforest.com/mobile-atlas/%z%/%x%/%y%.png?apikey=' + ThunderForest_ApiKey, 0, 19, 4, nil);
    AddMapProvider('ThunderForest Transport-Dark', ptEPSG3857, 'https://tile.thunderforest.com/transport-dark/%z%/%x%/%y%.png?apikey=' + ThunderForest_ApiKey, 0, 19, 4, nil);
    AddMapProvider('ThunderForest Spinal-Map', ptEPSG3857, 'https://tile.thunderforest.com/spinal-map/%z%/%x%/%y%.png?apikey=' + ThunderForest_ApiKey, 0, 19, 4, nil);

    // The following providers could be used alternatively. No API key required,
    // but has a gray "API Key required" watermark and maybe other restrictions!
    //   AddMapProvider('Open Cycle Map', ptEPSG3857, 'http://%serv%.tile.opencyclemap.org/cycle/%z%/%x%/%y%.png', 0, 18, 3, @GetSvrLetter);
    //   AddMapProvider('OpenStreetMap Transport', ptEPSG3857, 'https://%serv%.tile.thunderforest.com/transport/%z%/%x%/%y%.png', 0, 18, 3, @GetSvrLetter);
  end;

  // Google
  AddMapProvider('Google Maps', ptEPSG3857, 'http://mt%serv%.google.com/vt/lyrs=m@145&v=w2.104&x=%x%&y=%y%&z=%z%', 0, 19, 4, nil);
  AddMapProvider('Google Satellite', ptEPSG3857, 'http://mt%serv%.google.com/vt/lyrs=y&hl=en&x=%x%&y=%y%&z=%z%' , 0, 19, 4, nil);
  // not working any more (June 2023), replaced by above:
  //AddMapProvider('Google Satellite', ptEPSG3857, 'http://khm%serv%.google.com/kh/v=863?x=%x%&y=%y%&z=%z%', 0, 19, 4, nil);
  AddMapProvider('Google Terrain', ptEPSG3857, 'http://mt%serv%.google.com/vt/lyrs=p&hl=en&x=%x%&y=%y%&z=%z%' , 0, 19, 4, nil);
  AddMapProvider('Google Satellite Only', ptEPSG3857, 'http://mt%serv%.google.com/vt/lyrs=s&hl=en&x=%x%&y=%y%&z=%z%' , 0, 19, 4, nil);
  AddMapProvider('Google Altered Roadmap', ptEPSG3857, 'http://mt%serv%.google.com/vt/lyrs=r&hl=en&x=%x%&y=%y%&z=%z%' , 0, 19, 4, nil);
  AddMapProvider('Google Roadmap', ptEPSG3857, 'http://mt%serv%.google.com/vt/lyrs=m&hl=en&x=%x%&y=%y%&z=%z%' , 0, 19, 4, nil);

  // Yandex
  AddMapProvider('Yandex.Maps', ptEPSG3395, 'https://core-renderer-tiles.maps.yandex.net/tiles?l=map&x=%x%&y=%y%&z=%z%&scale=1', 0, 19, 4, nil, nil, nil, nil);  // Russion labels
  AddMapProvider('Yandex.Maps Satellite', ptEPSG3395, 'https://core-sat.maps.yandex.net/tiles?l=sat&x=%x%&y=%y%&z=%z%', 0, 19, 4, nil, nil, nil, nil);
  AddMapProvider('Yandex.Maps Satellite-old', ptEPSG3395, 'https://sat0%serv%.maps.yandex.net/tiles?l=sat&x=%x%&y=%y%&z=%z%', 0, 19, 4, @GetSvrBase1, nil, nil, nil);
  // The next ones are no longer valid. Keeping them here just in case ...
  //AddMapProvider('Yandex.Maps-old', ptEPSG3395, 'https://vec0%serv%.maps.yandex.net/tiles?l=map&x=%x%&y=%y%&z=%z%&scale=1&lang=ru_RU', 0, 19, 4, @GetSvrBase1, nil, nil, nil);

  // Bing
  AddMapProvider('Virtual Earth Bing', ptEPSG3857, 'http://ecn.t%serv%.tiles.virtualearth.net/tiles/r%x%?g=671&mkt=en-us&lbl=l1&stl=h&shading=hill', 1, 19, 8, nil, @GetStrQuadKey);
  AddMapProvider('Virtual Earth Aerial', ptEPSG3857, 'http://a%serv%.ortho.tiles.virtualearth.net/tiles/a%x%.jpg?g=72&shading=hill', 1, 19, 4, nil, @GetStrQuadKey);
  AddMapProvider('Virtual Earth Hybrid', ptEPSG3857, 'http://h%serv%.ortho.tiles.virtualearth.net/tiles/h%x%.jpg?g=72&shading=hill', 1, 19, 4, nil, @GetStrQuadKey);

  // 2GIS    (Russian labels, limited zoom range)
  AddMapProvider('2GIS', ptEPSG3857, 'http://tile%serv%.maps.2gis.com/tiles?x=%x%&y=%y%&z=%z%', 2, 18, 4, nil);  // Reduced zoom levels tested...

  if (HERE_AppID <> '') and (HERE_AppCode <> '') then begin
    // Registration required to access HERE maps:
    //   https://developer.here.com/?create=Freemium-Basic&keepState=true&step=account
    // Store the APP_ID and APP_CODE obtained after registration in the
    // ini file of the demo under key [HERE] as items APP_ID and APP_CODE and
    // restart the demo.
    HERE1 := 'http://%serv%.base.maps.api.here.com/maptile/2.1/maptile/newest/';
    HERE2 := '/%z%/%x%/%y%/256/png8?app_id=' + HERE_AppID + '&app_code=' + HERE_AppCode;
    AddMapProvider('Here WeGo Map', ptEPSG3857, HERE1 + 'normal.day' + HERE2, 1, 19, 4, @GetSvrBase1);
    AddMapProvider('Here WeGo Grey Map', ptEPSG3857, HERE1 + 'normal.day.grey' + HERE2, 1, 19, 4, @GetSvrBase1);
    AddMapProvider('Here WeGo Reduced Map', ptEPSG3857, HERE1 + 'reduced.day' + HERE2, 1, 19, 4, @GetSvrBase1);
    AddMapProvider('Here WeGo Transit Map', ptEPSG3857, HERE1 + 'normal.day.transit' + HERE2, 1, 19, 4, @GetSvrBase1);
    AddMapProvider('Here WeGo POI Map', ptEPSG3857, HERE1 + 'normal.day' + HERE2 + '&pois', 1, 19, 4, @GetSvrBase1);
    AddMapProvider('Here WeGo Pedestrian Map', ptEPSG3857, HERE1 + 'pedestrian.day' + HERE2, 1, 19, 4, @GetSvrBase1);
    AddMapProvider('Here WeGo DreamWorks Map', ptEPSG3857, HERE1 + 'normal.day' + HERE2 + '&style=dreamworks', 1, 19, 4, @GetSvrBase1);
  end;

  if (OpenWeatherMap_ApiKey <> '') then begin
    // Registration required to access OpenWeatherMaps
    //   https://home.openweathermap.org/users/sign_up
    // Store the API key found on the website in the ini file of the demo under key [OpenWeatherMap] and API_Key and restart the demo
    AddMapProvider('OpenWeatherMap Clouds', ptEPSG3857, 'https://tile.openweathermap.org/map/clouds_new/%z%/%x%/%y%.png?appid=' + OpenWeatherMap_ApiKey, 1, 19, 1, nil);
    AddMapProvider('OpenWeatherMap Precipitation', ptEPSG3857, 'https://tile.openweathermap.org/map/precipitation_new/%z%/%x%/%y%.png?appid=' + OpenWeatherMap_ApiKey, 1, 19, 1, nil);
    AddMapProvider('OpenWeatherMap Pressure', ptEPSG3857, 'https://tile.openweathermap.org/map/pressure_new/%z%/%x%/%y%.png?appid=' + OpenWeatherMap_ApiKey, 1, 19, 1, nil);
    AddMapProvider('OpenWeatherMap Temperature', ptEPSG3857, 'https://tile.openweathermap.org/map/temp_new/%z%/%x%/%y%.png?appid=' + OpenWeatherMap_ApiKey, 1, 19, 1, nil);
    AddMapProvider('OpenWeatherMap Wind', ptEPSG3857, 'https://tile.openweathermap.org/map/wind_new/%z%/%x%/%y%.png?appid=' + OpenWeatherMap_ApiKey, 1, 19, 1, nil);
  end;

  {
  // The following maps need hybrid overlays
  AddMapProvider('Google Hybrid', ptEPSG3857, 'http://mt%serv%.google.com/vt/lyrs=h@145&v=w2.104&x=%x%&y=%y%&z=%z%', 0, 19, 4, nil);
  AddMapProvider('Google Physical', ptEPSG3857, 'http://mt%serv%.google.com/vt/lyrs=t@145&v=w2.104&x=%x%&y=%y%&z=%z%', 0, 19, 4, nil);
  AddMapProvider('Yandex.Maps Hybrid', ptEPSG3395, 'https://vec0%serv%.maps.yandex.net/tiles?l=skl&x=%x%&y=%y%&z=%z%', 0, 19, 4, @GetSvrBase1, nil, nil, nil);
  }

  // ArcGIS
  AddMapProvider('ArcGIS World Street Map', ptEPSG3857, 'http://server.arcgisonline.com/ArcGIS/rest/services/World_Street_Map/MapServer/tile/%z%/%y%/%x%.jpg', 0, 19, 4, nil);
  AddMapProvider('ArcGIS World Shaded Relief', ptEPSG3857, 'http://server.arcgisonline.com/ArcGIS/rest/services/World_Shaded_Relief/MapServer/tile/%z%/%y%/%x%.jpg', 0, 19, 4, nil);
//  AddMapProvider('ArcGIS World Physical Map', ptEPSG3857, 'http://server.arcgisonline.com/ArcGIS/rest/services/World_Physical_Map/MapServer/tile/%z%/%y%/%x%.jpg', 0, 19, 4, nil);   --- not yet available
  AddMapProvider('ArcGIS NatGeo World Map', ptEPSG3857, 'http://services.arcgisonline.com/ArcGIS/rest/services/NatGeo_World_Map/MapServer/tile/%z%/%y%/%x%', 0, 19, 4, nil);
//  AddMapProvider('ArcGIS Ocean Base', ptEPSG3857, 'http://services.arcgisonline.com/ArcGIS/rest/services/Ocean/World_Ocean_Base/MapServer/tile/%z%/%y%/%x%.jpg', 0, 19, 4, nil);  // --- not yet available
//  AddMapProvider('ArcGIS Imagery', ptEPSG3857, 'http://services.arcgisonline.com/ArcGIS/rest/services/Ocean/World_Ocean_Base/MapServer/tile/%z%/%y%/%x%.jpg', 0, 19, 4, nil);  // not available
  AddMapProvider('ArcGIS Clarity', ptEPSG3857, 'https://clarity.maptiles.arcgis.com/arcgis/rest/services/World_Imagery/MapServer/tile/%z%/%y%/%x%?blankTile=false', 0, 19, 4, nil);

  // Apple
  //  AddMapProvider('GSP2 Apple', ptEPSG3857, 'http://gsp2.apple.com/tile?api=1&style=slideshow&layers=default&lang=de_DE&z=%z%&x=%x%&y=%y%&v=9', 0, 19, 4, nil);

// CartoDB
  AddMapProvider('CartoDB Light All', ptEPSG3857, 'https://cartodb-basemaps-a.global.ssl.fastly.net/light_all/%z%/%x%/%y%.png', 0, 19, 4, nil);
  AddMapProvider('CartoDB Voyager', ptEPSG3857, 'https://cartodb-basemaps-a.global.ssl.fastly.net/rastertiles/voyager/%z%/%x%/%y%.png', 0, 19, 4, nil);

  // "Maps for free"
  AddMapProvider('Maps For Free', ptEPSG3857, 'http://maps-for-free.com/layer/relief/z%z%/row%y%/%z%_%x%-%y%.jpg', 0, 19, 4, nil);

  // MemoMaps
  AddMapProvider('Memo Maps', ptEPSG3857,'http://tile.memomaps.de/tilegen/%z%/%x%/%y%.png', 0, 19, 4, nil);

  // Sigma DC Control
  //AddMapProvider('Sigma DC Control', ptEPSG3857,'http://tiles1.sigma-dc-control.com/layer5/%z%/%x%/%y%.png', 0, 19, 4, nil);  // -- not working

  // Stamen
  AddMapProvider('Stamen Terrain', ptEPSG3857, 'http://tile.stamen.com/terrain/%z%/%x%/%y%.jpg', 0, 19, 4, nil);
  AddMapProvider('Stamen Watercolor', ptEPSG3857, 'https://stamen-tiles.a.ssl.fastly.net/watercolor/%z%/%x%/%y%.jpg' , 0, 19, 4, nil);

  // Via Michelin
  AddMapProvider('ViaMichelin', ptEPSG3857, 'http://map1.viamichelin.com/map/mapdirect?map=light&z=%z%&x=%x%&y=%y%&format=png&version=201503191157&layer=background', 0, 19, 4, nil);

  // GeoApify
  //AddMapProvider('Geoapify Map Tiles', ptEPSG3857, 'https://maps.geoapify.com/v1/tile/osm-bright-smooth/%z%/%x%/%y%.png' , 0, 19, 4, nil);     // -- not working

  // Stadia outdoors
  // AddMapProvider('Stadia Outdoors', ptEPSG3857, 'https://tiles.stadiamaps.com/tiles/outdoors/%z%/%x%/%y%.png', 0, 19, 4, nil);   -- subscription required

  // Tracestrack
  //AddMapProvider('Tracestrack Carto', ptEPSG3857, 'https://tile.tracestrack.com/en/%z%/%x%/%y%.png,' , 0, 19, 4, nil);      // -- not working

  // Waze
  AddMapProvider('Waze Background', ptEPSG3857, 'https://worldtiles1.waze.com/tiles/%z%/%x%/%y%.png', 0, 19, 4, nil);

