"use client";

import { I18nProvider, useI18n } from "@/lib/i18n";
import LanguageToggle from "@/components/LanguageToggle";

function MethodologyContent() {
  const { t, locale } = useI18n();

  if (locale === "en") {
    return <EnglishContent />;
  }
  return <GermanContent />;
}

function GermanContent() {
  const { t } = useI18n();
  return (
    <div className="min-h-screen bg-white">
      <Header />
      <main className="mx-auto max-w-3xl px-6 py-12">
        <h1 className="text-3xl font-bold text-slate-900 mb-2">{t("methodTitle")}</h1>
        <p className="text-lg text-slate-500 mb-10">{t("methodSubtitle")}</p>

        <div className="prose prose-slate max-w-none">
          <h2>Die Grundidee</h2>
          <p>
            Wie denken die Menschen in Ihrem Landkreis über Zuwanderung, Klimaschutz oder
            die Schuldenbremse? Normalerweise lässt sich das kaum sagen — Umfragen befragen
            meist nur einige tausend Personen in ganz Deutschland, viel zu wenige für
            verlässliche Aussagen auf Kreisebene.
          </p>
          <p>
            Meinungsbild löst dieses Problem mit einer Methode namens{" "}
            <strong>Multilevel Regression and Poststratification (MRP)</strong>.
            Vereinfacht gesagt funktioniert das so:
          </p>
          <ol>
            <li>
              Wir analysieren, wie Meinungen mit Merkmalen wie Alter, Geschlecht,
              Bildung und Wohnort zusammenhängen.
            </li>
            <li>
              Aus dem Zensus 2022 wissen wir, wie viele Menschen mit welchen
              Merkmalen in jedem Kreis leben.
            </li>
            <li>
              Durch die Kombination beider Informationen berechnen wir, welche
              Meinungsverteilung in jedem Kreis zu erwarten ist.
            </li>
          </ol>
          <p>
            MRP wird weltweit in der Politikwissenschaft eingesetzt und hat sich
            als zuverlässige Methode bewährt — etwa bei der Vorhersage des
            Brexit-Referendums auf Wahlkreisebene.
          </p>

          <h2>Unsere Daten</h2>

          <h3>Umfragen</h3>
          <p>
            Wir nutzen fünf große deutsche Umfrageprogramme mit insgesamt rund{" "}
            <strong>118.000 Befragten</strong> und über{" "}
            <strong>1,7 Millionen</strong> Antworten zu 43 politischen Themen:
          </p>
          <ul>
            <li>
              <strong>GLES Tracking</strong> — Fortlaufende Befragung zur Bundestagswahl (2009–2023)
            </li>
            <li>
              <strong>GLES Querschnitt & Rolling Cross-Section 2025</strong> — Befragungen zur Bundestagswahl 2025
            </li>
            <li>
              <strong>GLES Kumulation</strong> — Zusammengefasste Daten mehrerer Wahlstudien
            </li>
            <li>
              <strong>ALLBUS</strong> — Allgemeine Bevölkerungsumfrage der Sozialwissenschaften
            </li>
          </ul>

          <h3>Zensus 2022</h3>
          <p>
            Vom Statistischen Bundesamt kennen wir die genaue Zusammensetzung der
            Bevölkerung in jedem Kreis — aufgeteilt nach Alter, Geschlecht und
            Bildungsabschluss. Das sind rund 400 Kreise mit je 50 demografischen
            Gruppen.
          </p>

          <h3>Wahlergebnisse</h3>
          <p>
            Zusätzlich verwenden wir Ergebnisse der letzten Bundestagswahl auf
            Kreisebene (Stimmenanteile von AfD und CDU/CSU, Wahlbeteiligung) sowie
            die Bevölkerungsdichte. Diese Informationen helfen dem Modell, regionale
            Meinungsunterschiede besser zu erfassen.
          </p>

          <h2>Das Modell</h2>
          <p>
            Für jedes der 43 Themen schätzen wir ein eigenes statistisches Modell.
            Im Kern ist es eine logistische Regression mit mehreren Ebenen:
          </p>

          <h3>Individuelle Merkmale</h3>
          <p>
            Das Modell berücksichtigt, wie sich Meinungen nach Alter, Geschlecht und
            Bildung unterscheiden — und wie diese Merkmale zusammenwirken. Zum Beispiel
            können junge Männer und ältere Frauen bei manchen Themen ähnlich denken,
            während junge Frauen und ältere Männer ganz andere Positionen vertreten.
          </p>

          <h3>Regionale Unterschiede</h3>
          <p>
            Auf drei geografischen Ebenen werden regionale Unterschiede modelliert:
            Bundesland, Landkreis und Bundestagswahlkreis. Zusätzlich fließen
            Wahlergebnisse und Bevölkerungsdichte ein, um auch Kreise gut schätzen zu
            können, aus denen nur wenige Befragte stammen.
          </p>

          <h3>Umfrage-Effekte</h3>
          <p>
            Da wir Daten aus verschiedenen Umfragen zusammenführen, berücksichtigt
            das Modell systematische Unterschiede zwischen den Umfrageprogrammen
            (z.B. durch unterschiedliche Befragungsmethoden) und zwischen
            Legislaturperioden.
          </p>

          <h3>Von Modell zu Karte</h3>
          <p>
            Im letzten Schritt werden die Modellergebnisse auf die Zensusdaten
            angewendet: Für jeden Kreis berechnen wir den Bevölkerungsdurchschnitt
            der vorhergesagten Zustimmung, gewichtet nach der tatsächlichen
            demografischen Zusammensetzung.
          </p>

          <h2>Drei geografische Ebenen</h2>
          <ul>
            <li>
              <strong>Bundesländer</strong> (16) — höchste Zuverlässigkeit, da viele
              Befragte pro Land
            </li>
            <li>
              <strong>Landkreise</strong> (400) — feinste räumliche Auflösung
            </li>
            <li>
              <strong>Bundestagswahlkreise</strong> (299) — politisch besonders relevant,
              da hier die Direktmandate vergeben werden
            </li>
          </ul>

          <h2>Wie gut sind die Schätzungen?</h2>
          <p>
            Wir überprüfen unsere Kreisschätzungen, indem wir sie auf Landesebene
            zusammenfassen und mit direkten Umfrageergebnissen vergleichen. Auf dieser
            Ebene haben Umfragen genug Befragte für verlässliche Vergleichswerte.
          </p>
          <p>
            Über alle 43 Themen erreichen wir eine{" "}
            <strong>mediane Korrelation von 0,88</strong> — das heißt, unsere
            Schätzungen stimmen gut mit den tatsächlichen Umfragewerten überein.
            Besonders gut funktioniert die Methode bei Themen wie Zuwanderung,
            Kernenergie und Mietpreisregulierung. Bei einigen Themen (z.B.
            Einschätzung der eigenen wirtschaftlichen Lage) sind die
            Schätzungen weniger zuverlässig.
          </p>

          <h2>Was man beachten sollte</h2>
          <ul>
            <li>
              Die Schätzungen sind Modellvorhersagen, keine direkten Befragungsergebnisse.
              Für Kreise mit wenigen oder keinen Befragten stützt sich das Modell stärker
              auf demografische und regionale Muster.
            </li>
            <li>
              Die Qualität variiert je nach Thema. Bei Themen mit starken
              demografischen und regionalen Mustern (z.B. Zuwanderung) sind die
              Schätzungen zuverlässiger als bei Themen, die sich nur schwach nach
              Region unterscheiden.
            </li>
            <li>
              Derzeit werden keine Unsicherheitsintervalle angezeigt. Die Schätzwerte
              sollten daher als bestmögliche Annäherung verstanden werden, nicht als
              exakte Werte.
            </li>
          </ul>

          <h2>Daten & Software</h2>
          <p>
            Alle Analysen werden mit R durchgeführt (Paket <code>lme4</code>).
            Die Website nutzt Next.js und MapLibre GL.
          </p>
          <p>
            Die Umfragedaten stammen von{" "}
            <a href="https://www.gesis.org" target="_blank" rel="noopener noreferrer">
              GESIS — Leibniz-Institut für Sozialwissenschaften
            </a>
            . Die Zensusdaten stammen vom Statistischen Bundesamt. Die Wahlergebnisse stammen
            aus der <a href="https://github.com/vhesener/german_election_data" target="_blank" rel="noopener noreferrer">GERDA</a>-Datenbank.
          </p>

          <h2>Literatur</h2>
          <ul className="text-sm">
            <li>
              Ghitza, Y. & Gelman, A. (2013). Deep Interactions with MRP.{" "}
              <em>American Journal of Political Science</em>, 57(3), 762–776.
            </li>
            <li>
              Goplerud, M. (2024). Re-evaluating Machine Learning for MRP.{" "}
              <em>Political Analysis</em>, 32(2), 1–19.
            </li>
            <li>
              Selb, P. & Munzert, S. (2011). Estimating Constituency Preferences from Sparse Survey Data Using Auxiliary Geographic Information.{" "}
              <em>Political Analysis</em>, 19(4), 455–470.
            </li>
          </ul>
        </div>
      </main>
    </div>
  );
}

function EnglishContent() {
  const { t } = useI18n();
  return (
    <div className="min-h-screen bg-white">
      <Header />
      <main className="mx-auto max-w-3xl px-6 py-12">
        <h1 className="text-3xl font-bold text-slate-900 mb-2">{t("methodTitle")}</h1>
        <p className="text-lg text-slate-500 mb-10">{t("methodSubtitle")}</p>

        <div className="prose prose-slate max-w-none">
          <h2>The Basic Idea</h2>
          <p>
            What do people in your county think about immigration, climate policy, or
            the debt brake? Normally, it&apos;s hard to say — surveys typically interview
            only a few thousand people across all of Germany, far too few for reliable
            estimates at the county level.
          </p>
          <p>
            Meinungsbild solves this problem using a method called{" "}
            <strong>Multilevel Regression and Poststratification (MRP)</strong>.
            In simple terms, it works like this:
          </p>
          <ol>
            <li>
              We analyze how opinions relate to characteristics like age, sex,
              education, and place of residence.
            </li>
            <li>
              From the 2022 Census, we know exactly how many people with which
              characteristics live in each county.
            </li>
            <li>
              By combining both pieces of information, we calculate the expected
              distribution of opinions in each county.
            </li>
          </ol>
          <p>
            MRP is widely used in political science around the world and has
            proven reliable — for example, in predicting the Brexit referendum
            results at the constituency level.
          </p>

          <h2>Our Data</h2>

          <h3>Surveys</h3>
          <p>
            We draw on five major German survey programs with a total of approximately{" "}
            <strong>118,000 respondents</strong> and over{" "}
            <strong>1.7 million</strong> responses across 43 political issues:
          </p>
          <ul>
            <li>
              <strong>GLES Tracking</strong> — Continuous federal election survey (2009–2023)
            </li>
            <li>
              <strong>GLES Cross-Section & Rolling Cross-Section 2025</strong> — Surveys for the 2025 federal election
            </li>
            <li>
              <strong>GLES Cumulation</strong> — Pooled data from multiple election studies
            </li>
            <li>
              <strong>ALLBUS</strong> — German General Social Survey
            </li>
          </ul>

          <h3>Census 2022</h3>
          <p>
            From the Federal Statistical Office, we know the exact demographic
            composition of each county — broken down by age, sex, and educational
            attainment. That&apos;s roughly 400 counties with 50 demographic groups each.
          </p>

          <h3>Election Results</h3>
          <p>
            We also use results from the most recent federal election at the county
            level (vote shares for the AfD and CDU/CSU, voter turnout) as well as
            population density. This information helps the model capture regional
            differences in opinion.
          </p>

          <h2>The Model</h2>
          <p>
            For each of the 43 issues, we estimate a separate statistical model. At
            its core, it is a multilevel logistic regression:
          </p>

          <h3>Individual Characteristics</h3>
          <p>
            The model accounts for how opinions differ by age, sex, and education —
            and how these characteristics interact. For example, young men and older
            women may think similarly on some issues, while young women and older men
            hold quite different positions.
          </p>

          <h3>Regional Differences</h3>
          <p>
            Regional differences are modeled at three geographic levels: federal
            state, county, and electoral district. In addition, election results
            and population density help the model produce good estimates even for
            counties with few or no survey respondents.
          </p>

          <h3>Survey Effects</h3>
          <p>
            Since we pool data from different surveys, the model accounts for
            systematic differences between survey programs (e.g., due to different
            survey modes) and between legislative periods.
          </p>

          <h3>From Model to Map</h3>
          <p>
            In the final step, the model results are applied to the census data:
            for each county, we compute the population-weighted average of the
            predicted support, weighted by the actual demographic composition.
          </p>

          <h2>Three Geographic Levels</h2>
          <ul>
            <li>
              <strong>Federal states</strong> (16) — highest reliability, with many
              respondents per state
            </li>
            <li>
              <strong>Counties</strong> (400) — finest spatial resolution
            </li>
            <li>
              <strong>Electoral districts</strong> (299) — politically relevant,
              as this is where constituency seats are decided
            </li>
          </ul>

          <h2>How Good Are the Estimates?</h2>
          <p>
            We validate our county-level estimates by aggregating them to the state
            level and comparing with direct survey results. At this level, surveys
            have enough respondents for reliable benchmarks.
          </p>
          <p>
            Across all 43 issues, we achieve a{" "}
            <strong>median correlation of 0.88</strong> — meaning our estimates
            align well with actual survey results. The method works particularly
            well for issues like immigration, nuclear energy, and rent regulation.
            For some issues (e.g., assessments of one&apos;s personal economic
            situation), the estimates are less reliable.
          </p>

          <h2>What to Keep in Mind</h2>
          <ul>
            <li>
              The estimates are model predictions, not direct survey results.
              For counties with few or no respondents, the model relies more heavily
              on demographic and regional patterns.
            </li>
            <li>
              Quality varies by issue. For issues with strong demographic and regional
              patterns (e.g., immigration), estimates are more reliable than for issues
              that vary little across regions.
            </li>
            <li>
              Currently, no uncertainty intervals are displayed. The estimates should
              therefore be understood as best approximations, not exact values.
            </li>
          </ul>

          <h2>Data & Software</h2>
          <p>
            All analyses are conducted in R (package <code>lme4</code>). The website
            is built with Next.js and MapLibre GL.
          </p>
          <p>
            Survey data are provided by{" "}
            <a href="https://www.gesis.org" target="_blank" rel="noopener noreferrer">
              GESIS — Leibniz Institute for the Social Sciences
            </a>
            . Census data come from the Federal Statistical Office of Germany.
            Election results come from the{" "}
            <a href="https://github.com/vhesener/german_election_data" target="_blank" rel="noopener noreferrer">
              GERDA
            </a>{" "}
            database.
          </p>

          <h2>References</h2>
          <ul className="text-sm">
            <li>
              Ghitza, Y. & Gelman, A. (2013). Deep Interactions with MRP.{" "}
              <em>American Journal of Political Science</em>, 57(3), 762–776.
            </li>
            <li>
              Goplerud, M. (2024). Re-evaluating Machine Learning for MRP.{" "}
              <em>Political Analysis</em>, 32(2), 1–19.
            </li>
            <li>
              Selb, P. & Munzert, S. (2011). Estimating Constituency Preferences from Sparse Survey Data Using Auxiliary Geographic Information.{" "}
              <em>Political Analysis</em>, 19(4), 455–470.
            </li>
          </ul>
        </div>
      </main>
    </div>
  );
}

function Header() {
  const { t } = useI18n();
  return (
    <header className="border-b border-slate-200 bg-white/80 backdrop-blur-sm px-5 py-3">
      <div className="mx-auto max-w-3xl flex items-center justify-between">
        <a href="/" className="flex items-center gap-2.5 group">
          <div className="h-8 w-8 rounded-lg bg-gradient-to-br from-blue-600 to-blue-700 flex items-center justify-center shadow-sm">
            <span className="text-white font-bold text-sm">M</span>
          </div>
          <span className="text-lg font-bold text-slate-900 group-hover:text-blue-700 transition-colors">
            {t("siteTitle")}
          </span>
        </a>
        <div className="flex items-center gap-5">
          <nav className="flex gap-4 text-sm">
            <a href="/methodik" className="text-blue-700 font-medium">
              {t("navMethodology")}
            </a>
            <a href="/" className="text-slate-600 hover:text-blue-700 font-medium transition-colors">
              {t("backToMap")}
            </a>
          </nav>
          <div className="h-4 w-px bg-slate-200" />
          <LanguageToggle />
        </div>
      </div>
    </header>
  );
}

export default function MethodikPage() {
  return (
    <I18nProvider>
      <MethodologyContent />
    </I18nProvider>
  );
}
