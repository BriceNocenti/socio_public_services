"""
Génère tests/PC18mf_vars_originales.R à partir du fichier Excel pc18_labels_variables.xlsx.
Produit un character vector par thème (module), une variable par ligne, libellé en commentaire.
"""

import zipfile
import xml.etree.ElementTree as ET
from pathlib import Path
from collections import OrderedDict

XLSX = Path(__file__).parent / "pc18_labels_variables.xlsx"
OUT  = Path(__file__).parent / "PC18mf_vars_originales.R"

NS = "http://schemas.openxmlformats.org/spreadsheetml/2006/main"


def read_xlsx(path: Path) -> list[tuple[str, str, str]]:
    """Retourne liste de (variable, module, libelle) depuis la 1ère feuille."""
    with zipfile.ZipFile(path) as z:
        with z.open("xl/sharedStrings.xml") as f:
            tree = ET.parse(f)
            strings = [
                "".join(t.text or "" for t in si.iter(f"{{{NS}}}t"))
                for si in tree.findall(f".//{{{NS}}}si")
            ]
        with z.open("xl/worksheets/sheet1.xml") as f:
            tree = ET.parse(f)
            rows = tree.findall(f".//{{{NS}}}row")

    result = []
    for row in rows[1:]:  # skip header
        cells = row.findall(f"{{{NS}}}c")
        vals = []
        for c in cells:
            t = c.get("t")
            v = c.find(f"{{{NS}}}v")
            if v is not None and t == "s":
                vals.append(strings[int(v.text)])
            elif v is not None:
                vals.append(v.text)
            else:
                vals.append("")
        # pad to 3 cols
        while len(vals) < 3:
            vals.append("")
        var, module, label = vals[0].strip(), vals[1].strip(), vals[2].strip()
        if var and module:
            result.append((var, module, label))
    return result


def truncate_label(label: str, max_len: int = 90) -> str:
    """Tronque les libellés trop longs pour garder des lignes lisibles."""
    if len(label) <= max_len:
        return label
    return label[:max_len].rstrip() + "..."


def build_r_script(rows: list[tuple[str, str, str]]) -> str:
    # Regrouper par module en conservant l'ordre d'apparition
    modules: OrderedDict[str, list[tuple[str, str]]] = OrderedDict()
    for var, module, label in rows:
        modules.setdefault(module, []).append((var, label))

    lines = [
        "# ============================================================",
        "# PC18 — Variables originales de la base",
        "# Enquête Pratiques culturelles 2018 (Ministère de la Culture)",
        "# ============================================================",
        "#",
        "# Variables classées par thème (colonne « module » du questionnaire).",
        "# Libellé de la question en commentaire inline.",
        "# ============================================================",
        "",
    ]

    for module, vars_ in modules.items():
        # Largeur max du nom de variable dans ce bloc (pour alignement)
        max_name = max(len(v) for v, _ in vars_)
        pad = max_name + 2  # + guillemets

        lines.append("")
        lines.append(f"# {'=' * 60}")
        lines.append(f"# {module}")
        lines.append(f"# {'=' * 60}")
        lines.append("")
        lines.append(f"vars_{module.lower().replace(' ', '_').replace('-', '_').replace('é', 'e').replace('è', 'e').replace('ê', 'e').replace('â', 'a')} <- c(")

        for i, (var, label) in enumerate(vars_):
            comma = "," if i < len(vars_) - 1 else " "
            quoted = f'"{var}"'
            if label:
                short = truncate_label(label)
                line = f"  {quoted:<{pad}}{comma} # {short}"
            else:
                line = f"  {quoted:<{pad}}{comma}"
            lines.append(line)

        lines.append(")")

    return "\n".join(lines) + "\n"


def main() -> None:
    rows = read_xlsx(XLSX)
    script = build_r_script(rows)
    OUT.write_text(script, encoding="utf-8")
    print(f"Fichier généré : {OUT}")
    # Résumé
    from collections import Counter
    counts = Counter(m for _, m, _ in rows)
    for module, n in counts.items():
        print(f"  {module}: {n} variables")


if __name__ == "__main__":
    main()
