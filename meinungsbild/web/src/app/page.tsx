import dynamic from "next/dynamic";

const MeinungsbildMap = dynamic(() => import("@/components/MeinungsbildMap"), {
  ssr: false,
  loading: () => (
    <div className="flex h-[600px] items-center justify-center bg-zinc-100">
      <p className="text-zinc-500">Karte wird geladen...</p>
    </div>
  ),
});

export default function Home() {
  return (
    <div className="flex min-h-screen flex-col bg-white">
      <header className="border-b border-zinc-200 px-6 py-4">
        <div className="mx-auto flex max-w-7xl items-center justify-between">
          <div>
            <h1 className="text-2xl font-bold tracking-tight text-zinc-900">
              Meinungsbild
            </h1>
            <p className="text-sm text-zinc-500">
              Was Deutschland denkt — Regionale Meinungsschätzungen
            </p>
          </div>
          <nav className="flex gap-6 text-sm font-medium text-zinc-600">
            <a href="/methodik" className="hover:text-zinc-900">
              Methodik
            </a>
            <a href="/ueber" className="hover:text-zinc-900">
              Über
            </a>
            <a href="/daten" className="hover:text-zinc-900">
              Daten
            </a>
          </nav>
        </div>
      </header>

      <main className="flex flex-1 flex-col">
        <div className="mx-auto w-full max-w-7xl px-6 py-6">
          <div className="mb-4 flex flex-wrap items-center gap-4">
            <IssueSelector />
            <GeographySelector />
          </div>
        </div>
        <div className="flex-1">
          <MeinungsbildMap />
        </div>
      </main>

      <footer className="border-t border-zinc-200 px-6 py-4 text-center text-xs text-zinc-400">
        Meinungsbild — Heddesheimer et al. | Daten:{" "}
        <a
          href="https://www.german-elections.com"
          className="underline hover:text-zinc-600"
        >
          GERDA
        </a>
        , GESIS, Zensus 2022
      </footer>
    </div>
  );
}

function IssueSelector() {
  return (
    <select className="rounded-md border border-zinc-300 bg-white px-3 py-2 text-sm text-zinc-700 shadow-sm">
      <option value="">Thema wählen...</option>
      <option value="immigration_level">Zuwanderung</option>
      <option value="speed_limit">Tempolimit</option>
      <option value="eu_membership">EU-Mitgliedschaft</option>
      <option value="climate_policy">Klimapolitik</option>
      <option value="minimum_wage">Mindestlohn</option>
      <option value="defense_spending">Verteidigungsausgaben</option>
      <option value="cannabis">Cannabis-Legalisierung</option>
      <option value="rent_control">Mietpreisbremse</option>
    </select>
  );
}

function GeographySelector() {
  return (
    <div className="flex gap-1 rounded-md border border-zinc-300 bg-zinc-50 p-0.5 text-sm">
      <button className="rounded bg-white px-3 py-1.5 font-medium text-zinc-900 shadow-sm">
        Kreise
      </button>
      <button className="rounded px-3 py-1.5 text-zinc-500 hover:text-zinc-700">
        Bundesländer
      </button>
      <button className="rounded px-3 py-1.5 text-zinc-500 hover:text-zinc-700">
        Wahlkreise
      </button>
    </div>
  );
}
