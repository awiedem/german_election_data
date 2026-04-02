"use client";

import { useRef, useEffect, useState, useCallback } from "react";
import maplibregl from "maplibre-gl";
import "maplibre-gl/dist/maplibre-gl.css";
import { choroplethExpression } from "@/lib/colors";
import type { GeoLevel, KreisEstimate, BundeslandEstimate, WkrEstimate } from "@/lib/types";

interface MapProps {
  geoLevel: GeoLevel;
  estimates: Record<string, KreisEstimate[] | BundeslandEstimate[] | WkrEstimate[]>;
  selectedIssue: string;
  onRegionHover?: (info: { name: string; estimate: number; code: string } | null) => void;
  dataRange: { min: number; max: number };
}

const GERMANY_CENTER: [number, number] = [10.4, 51.1];
const GERMANY_ZOOM = 5.3;

export default function Map({ geoLevel, estimates, selectedIssue, onRegionHover, dataRange }: MapProps) {
  const containerRef = useRef<HTMLDivElement>(null);
  const mapRef = useRef<maplibregl.Map | null>(null);
  const popupRef = useRef<maplibregl.Popup | null>(null);
  const [loaded, setLoaded] = useState(false);

  // Initialize map
  useEffect(() => {
    if (!containerRef.current || mapRef.current) return;

    const map = new maplibregl.Map({
      container: containerRef.current,
      style: {
        version: 8,
        sources: {},
        layers: [
          {
            id: "background",
            type: "background",
            paint: { "background-color": "#f8fafc" },
          },
        ],
      },
      center: GERMANY_CENTER,
      zoom: GERMANY_ZOOM,
      minZoom: 4,
      maxZoom: 12,
      attributionControl: false,
    });

    map.addControl(new maplibregl.NavigationControl(), "top-right");

    map.on("load", () => {
      // Add Kreise source
      map.addSource("kreise", {
        type: "geojson",
        data: "/data/kreise.geojson",
      });

      // Add Bundesländer source
      map.addSource("bundeslaender", {
        type: "geojson",
        data: "/data/bundeslaender.geojson",
      });

      // Add Wahlkreise source
      map.addSource("wahlkreise", {
        type: "geojson",
        data: "/data/wahlkreise.geojson",
      });

      // Kreise fill layer
      map.addLayer({
        id: "kreise-fill",
        type: "fill",
        source: "kreise",
        paint: {
          "fill-color": "#e5e7eb",
          "fill-opacity": 0.85,
        },
        layout: { visibility: "none" },
      });

      // Kreise outline
      map.addLayer({
        id: "kreise-line",
        type: "line",
        source: "kreise",
        paint: {
          "line-color": "#94a3b8",
          "line-width": 0.5,
        },
        layout: { visibility: "none" },
      });

      // Bundesländer fill layer
      map.addLayer({
        id: "bundeslaender-fill",
        type: "fill",
        source: "bundeslaender",
        paint: {
          "fill-color": "#e5e7eb",
          "fill-opacity": 0.85,
        },
        layout: { visibility: "none" },
      });

      // Bundesländer outline
      map.addLayer({
        id: "bundeslaender-line",
        type: "line",
        source: "bundeslaender",
        paint: {
          "line-color": "#475569",
          "line-width": 1.5,
        },
        layout: { visibility: "none" },
      });

      // Wahlkreise fill layer
      map.addLayer({
        id: "wahlkreise-fill",
        type: "fill",
        source: "wahlkreise",
        paint: {
          "fill-color": "#e5e7eb",
          "fill-opacity": 0.85,
        },
        layout: { visibility: "none" },
      });

      // Wahlkreise outline
      map.addLayer({
        id: "wahlkreise-line",
        type: "line",
        source: "wahlkreise",
        paint: {
          "line-color": "#94a3b8",
          "line-width": 0.7,
        },
        layout: { visibility: "none" },
      });

      // State borders always visible (overlay)
      map.addLayer({
        id: "state-borders",
        type: "line",
        source: "bundeslaender",
        paint: {
          "line-color": "#1e293b",
          "line-width": 1,
        },
      });

      setLoaded(true);
    });

    popupRef.current = new maplibregl.Popup({
      closeButton: false,
      closeOnClick: false,
    });

    mapRef.current = map;

    return () => {
      map.remove();
      mapRef.current = null;
    };
  }, []);

  // Update layer visibility when geoLevel changes
  useEffect(() => {
    const map = mapRef.current;
    if (!map || !loaded) return;

    map.setLayoutProperty("kreise-fill", "visibility", geoLevel === "kreise" ? "visible" : "none");
    map.setLayoutProperty("kreise-line", "visibility", geoLevel === "kreise" ? "visible" : "none");
    map.setLayoutProperty("wahlkreise-fill", "visibility", geoLevel === "wahlkreise" ? "visible" : "none");
    map.setLayoutProperty("wahlkreise-line", "visibility", geoLevel === "wahlkreise" ? "visible" : "none");
    map.setLayoutProperty("bundeslaender-fill", "visibility", geoLevel === "bundeslaender" ? "visible" : "none");
    map.setLayoutProperty("bundeslaender-line", "visibility", geoLevel === "bundeslaender" ? "visible" : "none");
  }, [geoLevel, loaded]);

  // Update choropleth colors when estimates or issue changes
  const updateColors = useCallback(() => {
    const map = mapRef.current;
    if (!map || !loaded || !selectedIssue) return;

    const issueData = estimates[selectedIssue];
    if (!issueData) return;

    if (geoLevel === "kreise") {
      const colorMap: Record<string, number> = {};
      for (const d of issueData as KreisEstimate[]) {
        colorMap[d.county_code] = d.estimate;
      }
      map.setPaintProperty(
        "kreise-fill",
        "fill-color",
        choroplethExpression("county_code", colorMap, dataRange.min, dataRange.max) as maplibregl.ExpressionSpecification
      );
    } else if (geoLevel === "wahlkreise") {
      const colorMap: Record<string, number> = {};
      for (const d of issueData as WkrEstimate[]) {
        colorMap[String(d.wkr_nr)] = d.estimate;
      }
      map.setPaintProperty(
        "wahlkreise-fill",
        "fill-color",
        choroplethExpression("wkr_nr", colorMap, dataRange.min, dataRange.max, true) as maplibregl.ExpressionSpecification
      );
    } else {
      const colorMap: Record<string, number> = {};
      for (const d of issueData as BundeslandEstimate[]) {
        colorMap[d.state_code] = d.estimate;
      }
      map.setPaintProperty(
        "bundeslaender-fill",
        "fill-color",
        choroplethExpression("state_code", colorMap, dataRange.min, dataRange.max) as maplibregl.ExpressionSpecification
      );
    }
  }, [estimates, selectedIssue, geoLevel, loaded, dataRange]);

  useEffect(() => {
    updateColors();
  }, [updateColors]);

  // Hover interaction
  useEffect(() => {
    const map = mapRef.current;
    if (!map || !loaded) return;

    const layerId = geoLevel === "kreise" ? "kreise-fill" : geoLevel === "wahlkreise" ? "wahlkreise-fill" : "bundeslaender-fill";
    const codeProperty = geoLevel === "kreise" ? "county_code" : geoLevel === "wahlkreise" ? "wkr_nr" : "state_code";
    const nameProperty = "name";

    const onMove = (e: maplibregl.MapMouseEvent) => {
      const features = map.queryRenderedFeatures(e.point, { layers: [layerId] });
      map.getCanvas().style.cursor = features.length ? "pointer" : "";

      if (features.length > 0) {
        const f = features[0];
        const code = f.properties?.[codeProperty] as string;
        const name = f.properties?.[nameProperty] as string;
        const issueData = estimates[selectedIssue];
        let estimate = 0;

        if (issueData) {
          if (geoLevel === "kreise") {
            const match = (issueData as KreisEstimate[]).find((d) => d.county_code === code);
            if (match) estimate = match.estimate;
          } else if (geoLevel === "wahlkreise") {
            const match = (issueData as WkrEstimate[]).find((d) => d.wkr_nr === Number(code));
            if (match) estimate = match.estimate;
          } else {
            const match = (issueData as BundeslandEstimate[]).find((d) => d.state_code === code);
            if (match) estimate = match.estimate;
          }
        }

        popupRef.current
          ?.setLngLat(e.lngLat)
          .setHTML(
            `<div class="text-sm">
              <div class="font-semibold">${name}</div>
              <div>${(estimate * 100).toFixed(1)}%</div>
            </div>`
          )
          .addTo(map);

        onRegionHover?.({ name, estimate, code });
      } else {
        popupRef.current?.remove();
        onRegionHover?.(null);
      }
    };

    const onLeave = () => {
      map.getCanvas().style.cursor = "";
      popupRef.current?.remove();
      onRegionHover?.(null);
    };

    map.on("mousemove", layerId, onMove);
    map.on("mouseleave", layerId, onLeave);

    return () => {
      map.off("mousemove", layerId, onMove);
      map.off("mouseleave", layerId, onLeave);
    };
  }, [geoLevel, loaded, estimates, selectedIssue, onRegionHover]);

  return <div ref={containerRef} className="w-full h-full" />;
}
