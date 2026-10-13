"""Unit tests for the Hessami (2018) name parser (standard library only).

The strings are INVENTED: each mirrors a pattern found in the source, but the
source names losing candidates, who are private individuals, so none of its real
strings appear in this public repository. The parser was additionally checked
against an independent R implementation on all 4,345 source strings
(0 differences, 2026-10-01).

    python3 code/checks/test_he_hessami_names.py
"""
import importlib.util
from pathlib import Path
import unittest

ROOT = Path(__file__).resolve().parents[2]
spec = importlib.util.spec_from_file_location(
    "he_hessami", ROOT / "code/mayoral_elections/00_he_hessami_parse.py")
he = importlib.util.module_from_spec(spec)
spec.loader.exec_module(he)

LEX = frozenset({"peter", "wolfgang", "joseph", "katrin", "richard", "heinrich",
                 "hans", "harald", "bianca", "rüdiger", "günter", "oliver", "reinhold",
                 "helge", "tilo", "konrad", "werner", "gerhard", "frank", "uwe"})


def p(x):
    return he.parse_name(x, LEX)


class ParseNameTests(unittest.TestCase):
    def test_standard(self):
        r = p("Muster, Peter")
        self.assertEqual((r["last"], r["first"], r["title"], r["flag"]),
                         ("Muster", "Peter", "", "ok"))
        self.assertEqual(he.canonical_name(r), "Muster, Peter")

    def test_titles(self):
        r = p("Dr. Beispiel, Wolfgang")
        self.assertEqual((r["title"], r["last"]), ("Dr.", "Beispiel"))
        self.assertEqual(he.canonical_name(r), "Dr. Beispiel, Wolfgang")
        self.assertEqual(p("Prof. Dr. Beispiel, Joseph")["title"], "Prof. Dr.")
        self.assertEqual(p("Dr. Dr. Beispiel, Frank")["title"], "Dr. Dr.")
        self.assertEqual(p("Dr. jur. Beispiel, Katrin")["title"], "Dr. jur.")
        r = p("Beispiel, Dr. Gerhard")          # title before the given name
        self.assertEqual((r["title"], r["first"]), ("Dr.", "Gerhard"))
        self.assertEqual(he.canonical_name(r), "Dr. Beispiel, Gerhard")

    def test_whitespace(self):
        self.assertEqual(p("Dr.  Muster, Richard")["last"], "Muster")
        self.assertEqual(p("Muster,  Heinrich")["first"], "Heinrich")

    def test_particles_and_hyphens(self):
        self.assertEqual(p("von der Musterburg, Hans-Hilmar")["last"], "von der Musterburg")
        self.assertEqual(p("Dr. de Musté, Michael")["last"], "de Musté")
        self.assertEqual(p("Muster do Exemplo, Manuel")["last"], "Muster do Exemplo")
        r = p("Muster-Beispiel, Bianca-Maria")
        self.assertEqual((r["last"], r["first"]), ("Muster-Beispiel", "Bianca-Maria"))

    def test_keys(self):
        self.assertEqual(he.name_key(p("Weiß, Rüdiger")), "weiss|ruediger")
        self.assertEqual(he.name_key(p("Weiß, Rüdiger")), he.name_key(p("Weiss, Rüdiger")))
        self.assertEqual(he.name_key(p("Muster, Hans-Peter")), he.name_key(p("Muster, Hans Peter")))
        self.assertEqual(he.name_key(p("Dr. Beispiel, Wolfgang")), he.name_key(p("Beispiel, Wolfgang")))
        self.assertEqual(he.key_part("de Musté"), "de muste")

    def test_initials_and_suffix(self):
        self.assertEqual(p("Muster, Oliver G.")["first"], "Oliver G.")
        r = p("Muster, Günter sen.")
        self.assertEqual((r["first"], r["suffix"]), ("Günter", "sen."))

    def test_separator_typos(self):
        r = p("Muster; Reinhold")
        self.assertEqual((r["last"], r["first"], r["flag"]), ("Muster", "Reinhold", "separator_fixed"))
        r = p("Muster. Helge")
        self.assertEqual((r["last"], r["first"], r["flag"]), ("Muster", "Helge", "separator_fixed"))
        self.assertEqual(p("Dr. Muster")["flag"], "last_name_only")   # a title is no separator

    def test_comma_less_order(self):
        r = p("Mustermann Tilo")
        self.assertEqual((r["last"], r["first"], r["flag"]), ("Mustermann", "Tilo", "no_comma_last_first"))
        r = p("Hans-Joachim Mustermann")
        self.assertEqual((r["last"], r["first"], r["flag"]),
                         ("Mustermann", "Hans-Joachim", "no_comma_first_last"))
        self.assertEqual(p("Konrad Werner")["flag"], "no_comma_order_ambiguous")

    def test_surname_only_and_merged_cell(self):
        r = p("Mustermann")
        self.assertEqual((r["last"], r["first"], r["flag"]), ("Mustermann", "", "last_name_only"))
        self.assertEqual(he.name_key(r), "mustermann|")
        r = p("Muster, Hans-PeterBeispiel, Uwe")
        self.assertEqual((r["last"], r["first"], r["flag"]),
                         ("Muster", "Hans-Peter", "merged_cell_truncated"))

    def test_empty(self):
        self.assertEqual(p("")["flag"], "empty")
        self.assertEqual(he.canonical_name(p("")), "")

    def test_lexicon(self):
        lex = he.build_lexicon(["Muster, Hans-Peter", "Dr. Beispiel, Dr. Anna Lena", "Kein Komma"])
        self.assertEqual(lex, frozenset({"hans", "anna"}))


if __name__ == "__main__":
    unittest.main()
