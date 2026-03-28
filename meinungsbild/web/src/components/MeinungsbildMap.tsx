"use client";

import { useEffect, useRef } from "react";
import maplibregl from "maplibre-gl";
import "maplibre-gl/dist/maplibre-gl.css";

export default function MeinungsbildMap() {
  const mapContainer = useRef<HTMLDivElement>(null);
  const map = useRef<maplibregl.Map | null>(null);

  useEffect(() => {
    if (!mapContainer.current || map.current) return;

    map.current = new maplibregl.Map({
      container: mapContainer.current,
      style: {
        version: 8,
        sources: {
          "carto-light": {
            type: "raster",
            tiles: [
              "https://basemaps.cartocdn.com/light_nolabels/{z}/{x}/{y}@2x.png",
            ],
            tileSize: 256,
            attribution:
              '&copy; <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a> &copy; <a href="https://carto.com/">CARTO</a>',
          },
        },
        layers: [
          {
            id: "carto-light-layer",
            type: "raster",
            source: "carto-light",
            minzoom: 0,
            maxzoom: 19,
          },
        ],
      },
      center: [10.4515, 51.1657], // Center of Germany
      zoom: 5.5,
      minZoom: 4,
      maxZoom: 12,
    });

    map.current.addControl(new maplibregl.NavigationControl(), "top-right");

    // TODO: Load PMTiles with Kreis boundaries and estimates
    // Once data is available:
    // 1. Add PMTiles source via protocol
    // 2. Add fill layer with data-driven colors (diverging choropleth)
    // 3. Add hover popup with estimate + CI
    // 4. Add click handler for detail navigation

    return () => {
      map.current?.remove();
      map.current = null;
    };
  }, []);

  return (
    <div
      ref={mapContainer}
      className="h-[600px] w-full"
      style={{ minHeight: "500px" }}
    />
  );
}
