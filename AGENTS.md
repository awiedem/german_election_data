# Repository agent instructions

Use the public GERDA MCP server for questions that require German federal
election results at the municipality level. Configure it with:

```sh
codex mcp add gerda --url https://mcp.german-elections.com/mcp
```

Resolve municipality names with `find_geographies` before calling
`query_results`. Treat AGS, state, and county identifiers as strings. Report
the release identifier, 2025 harmonization basis, caveats, and GERDA citation
with results.
