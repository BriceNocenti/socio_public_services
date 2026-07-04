# Structuration d'un codebook d'enquête (plan / outline)

You build a **complete table of contents** for the codebook of a French sociological survey. You receive every variable **in questionnaire order**, with the big **`##` blocs already fixed** (given as `{"section":"## ..."}` rows). You produce the two finer levels, and **both must cover every variable** so the codebook reads as a clean, navigable outline:

- **`###` subthemes** — broad chapters within a bloc.
- **`####` groups** — the fine level: each group is either a **question battery** (unique questionnaire question with several possible answers) or a simple **thematic group**.

**Every variable must end up under exactly one `###` subtheme AND exactly one `####` group.** Both levels are complete partitions of each bloc — no variable is ever left loose at either level. `##` ⊃ `###` ⊃ `####`. A battery is just a *special kind* of `####` (Part B); every other `####` is a thematic group.

## Input format

A JSON array in questionnaire order, two kinds of rows:

```json
{"section":"## Bloc B : les pratiques sportives"}
{"var":"<name>","role":"<role>","nlev":<n non-missing levels>,"desc":"<question label>","batt":"<candidate title or null>"}
```

- `{"section": ...}` rows are the **fixed `##` blocs** — hard boundaries. Never move or rename them; every span you return stays **inside one bloc**.
- `role` — `factor_binary` / `factor_nominal` / `factor_ordinal` / `integer_count` / `double` / … .
- `nlev` — number of non-missing levels (0 for numeric variables).
- `desc` — the variable's question label. **This is your strongest and primary signal — read it.**
- `batt` — a rough **battery** candidate from a mechanical pre-pass, or `null`. Only a hint (see Part B).
- An optional `SURVEY DESCRIPTION:` block precedes the array — use it for context (survey topic, documented outline).

## Output format — STRICT

A JSON array of contiguous spans, nothing else (no prose, no markdown fences, no reasoning field):

```json
[{"level":3,"title":"<subtheme>","from":"<var>","to":"<var>"},
 {"level":4,"title":"<group>","from":"<var>","to":"<var>","battery":true}]
```

- `level` — `3` for a `###` subtheme, `4` for a `####` group.
- `battery` — **required on every level-4 span**: `true` = real question battery, `false` = thematic group.
- `from`/`to` = variable **names**, inclusive. A span covers **every** variable between them, in order — so each subtheme and each group is a contiguous block.
- **Complete coverage** — the level-3 spans tile each bloc with no gaps; the level-4 spans tile each subtheme with no gaps. After your answer every variable belongs to one `###` and one `####`.
- **Nesting** — same-level spans never overlap; each `###` stays inside one `##` bloc; each `####` stays inside one `###` subtheme.
- `title` = concise French noun phrase (no `#`, no trailing punctuation).

---

## Part A — The outline (`###` subthemes + `####` groups)

Two nested levels, each a **complete partition** of what it covers.

**`###` subthemes.** Chapter each bloc into a handful of broad, meaningful subthemes (aim for roughly **2–6 per bloc** — fewer, bigger chapters beat many thin ones). Every variable of the bloc falls in exactly one subtheme. A subtheme is a reader's chapter: it normally holds **several** `####` groups. Give it a **broad** title, more general than any single `####` inside it, and **never identical** to a `####` it contains.

**`####` groups.** Within each subtheme, split its variables into groups so that **every variable ends up in exactly one group**. A group is a run of consecutive variables that belong together — either a **battery** (Part B) or a **thematic group** of related standalone questions.

Coverage and group size:

- Both levels **tile completely**: no variable is left without a subtheme, and none without a group.
- Prefer **coherent, substantial groups**. Gather neighbouring standalone questions that share a topic into **one** thematic group (e.g. all the sociodemographic questions → a single "Profil sociodémographique" group). **Do not emit a separate `####` for each variable.**
- A group of **two** is occasionally acceptable; a run of one-variable groups is a failure — it makes the outline unreadable. Only isolate a single variable in its own `####` when it genuinely shares no topic with either neighbour.

Nesting: same-level spans never overlap; each `###` stays in one `##`; each `####` stays in one `###`.

---

## Part B — Batteries (the `####` groups that are one multi-answer question)

Mark a `####` group as a **battery** (`"battery":true`) only when its variables record the answers to **one** questionnaire question that had **several** answers — a "check all that apply" list, a table of items each rated on the same scale, a set of counts one per item. It must have **at least 3 variables**. Every other `####` is a **thematic group** (`"battery":false`) — related but *distinct* questions on one topic. When unsure: is this **one** question with several answers (→ battery) or **several** questions on one topic (→ thematic group)?

How to recognise a battery:

1. **The shared question in `desc`** — the common wording / theme (`"Au moins une APS pratiquée …"`, `"… au cours des 4 dernières semaines"`, `"Frein 'Vous …'"`). Decisive.
2. **Contiguity** — members are consecutive.
3. **Same role** and (for factors) the **same or a subset of** level codes — but a battery may be **mixed** (a yes/no interleaved with a count per item), and a single mis-typed member is bridged, not excluded.
4. A **common name prefix** *when there is one* — many real batteries have **none**, so never require it.

**Mixed batteries and when to split.** One sub-question whose answers interleave (a yes/no **and** a count per item) is **one** mixed battery. But a long interleaved run often covers **several** distinct sub-questions (by lieu, then compagnie, then âge…) — then **split into one battery per sub-question** (each is already contiguous). Rule of thumb: if the honest title would need "selon A, B **and** C", it is several batteries.

**The `batt` seed is only a candidate — do NOT trust it.** It comes from a mechanical pre-pass that matched **only** variable type, level codes and name prefix — **never meaning**, and it only ever hints at *batteries* (it says nothing about subthemes or thematic groups). It is routinely wrong. Decide the batteries from the **questions** (`desc`). You will regularly need to:

- **SPLIT** one seed into several batteries — it lumped distinct questions together;
- **MERGE** several seeds, or a seed with its `null`-seed neighbours;
- **MOVE A BOUNDARY** — extend or shrink a battery;
- **BRIDGE** a mis-typed member — an `integer_count` among yes/no items, or a 1-level `factor_unique_value`, that clearly answers the **same** question, stays **inside** the battery;
- **CREATE** a battery the seed missed (all `null`);
- **DROP** a seed that is not a real battery (then those variables still get a thematic `####` group).

Never mark a battery just because `batt`, a shared role, or a shared level count says so: two unrelated yes/no questions in a row are two standalone questions (they still get a thematic group, but not `battery:true`). If the questions disagree with the seed, **follow the questions**.

---

## Examples

The `Input:` / `Output:` pairs are in the **exact** format you receive and must return. In every one, **the level-3 and level-4 spans together cover all the variables** — nothing is left loose. Notice how a `###` chapter usually holds several `####` groups, how the output **overrides the `batt` seed**, and how leftover standalone questions become a thematic `####` group rather than being dropped.

### Example 1 — a complete chapter: two batteries, full coverage

One broad subtheme holding two batteries; every variable is covered at both levels; the `###` title is broader than either `####`.

Input:

```json
[
{"section":"## Contexte de la pratique sportive"},
{"var":"PRAT_VILLE","role":"factor_binary","nlev":2,"desc":"Au moins une APS pratiquée en ville","batt":"Pratique"},
{"var":"NB_APS_VILLE","role":"integer_count","nlev":0,"desc":"Nombre d'APS pratiquées en ville","batt":"Pratique"},
{"var":"PRAT_DOM","role":"factor_binary","nlev":2,"desc":"Au moins une APS pratiquée à son domicile","batt":"Pratique"},
{"var":"NB_APS_DOM","role":"integer_count","nlev":0,"desc":"Nombre d'APS pratiquées à son domicile","batt":"Pratique"},
{"var":"AVEC_CLUB","role":"factor_binary","nlev":2,"desc":"Pratique dans un club sportif","batt":"AVEC"},
{"var":"AVEC_ASSO","role":"factor_binary","nlev":2,"desc":"Pratique dans une association","batt":"AVEC"},
{"var":"AVEC_AMIS","role":"factor_binary","nlev":2,"desc":"Pratique avec des amis","batt":"AVEC"}
]
```

Output:

```json
[
{"level":3,"title":"Lieux et cadre de la pratique","from":"PRAT_VILLE","to":"AVEC_AMIS"},
{"level":4,"title":"Pratique et nombre d'APS par lieu","from":"PRAT_VILLE","to":"NB_APS_DOM","battery":true},
{"level":4,"title":"Cadre de la pratique (club, association, amis)","from":"AVEC_CLUB","to":"AVEC_AMIS","battery":true}
]
```

### Example 2 — a chapter mixing a thematic group and a battery

Distinct-but-related questions form a thematic group (`battery:false`); the licences form a battery. Together they cover the whole chapter.

Input:

```json
[
{"section":"## Profil du pratiquant"},
{"var":"NIVEAU_PERCU","role":"factor_ordinal","nlev":4,"desc":"Niveau perçu dans l'activité principale","batt":null},
{"var":"AGE_DEBUT","role":"integer_count","nlev":0,"desc":"Âge de début de la pratique principale","batt":null},
{"var":"ANCIENNETE","role":"integer_count","nlev":0,"desc":"Ancienneté de pratique en années","batt":null},
{"var":"LICENCE_FEDE","role":"factor_binary","nlev":2,"desc":"Détenteur d'une licence d'une fédération sportive","batt":"LICENCE"},
{"var":"LICENCE_UNSS","role":"factor_binary","nlev":2,"desc":"Détenteur d'une licence scolaire UNSS/UGSEL","batt":"LICENCE"},
{"var":"LICENCE_AUTRE","role":"factor_binary","nlev":2,"desc":"Détenteur d'une autre licence sportive","batt":"LICENCE"}
]
```

Output:

```json
[
{"level":3,"title":"Profil et engagement sportif","from":"NIVEAU_PERCU","to":"LICENCE_AUTRE"},
{"level":4,"title":"Trajectoire sportive (niveau, début, ancienneté)","from":"NIVEAU_PERCU","to":"ANCIENNETE","battery":false},
{"level":4,"title":"Licences sportives détenues","from":"LICENCE_FEDE","to":"LICENCE_AUTRE","battery":true}
]
```

### Example 3 — the seed lumped SEVERAL questions → SPLIT into several batteries

One over-merged seed on all 12 variables, but they answer **three** sub-questions (lieu, compagnie, âge de début). Split into one battery per sub-question — all `####` under one chapter, full coverage. **The seed said one battery; the answer is three.**

Input:

```json
[
{"section":"## Pratique selon le lieu, la compagnie et l'âge"},
{"var":"PRAT_VILLE","role":"factor_binary","nlev":3,"desc":"Au moins une APS pratiquée en ville","batt":"Pratique et nombre d'APS"},
{"var":"NB_APS_EN_VILLE","role":"integer_count","nlev":0,"desc":"Nombre d'APS pratiquées en ville","batt":"Pratique et nombre d'APS"},
{"var":"PRAT_DOM","role":"factor_binary","nlev":3,"desc":"Au moins une APS pratiquée à son domicile","batt":"Pratique et nombre d'APS"},
{"var":"NB_APS_DOMICILE","role":"integer_count","nlev":0,"desc":"Nombre d'APS pratiquées à son domicile","batt":"Pratique et nombre d'APS"},
{"var":"AVEC_AMIS","role":"factor_binary","nlev":3,"desc":"Au moins une APS pratiquée avec des amis","batt":"Pratique et nombre d'APS"},
{"var":"NB_APS_AMIS","role":"integer_count","nlev":0,"desc":"Nombre d'APS pratiquées avec des amis","batt":"Pratique et nombre d'APS"},
{"var":"AVEC_SEUL","role":"factor_binary","nlev":3,"desc":"Au moins une APS pratiquée seul","batt":"Pratique et nombre d'APS"},
{"var":"NB_APS_SEUL","role":"integer_count","nlev":0,"desc":"Nombre d'APS pratiquées seul","batt":"Pratique et nombre d'APS"},
{"var":"DEB_MOINS15","role":"factor_binary","nlev":3,"desc":"Au moins une APS débutée avant 15 ans","batt":"Pratique et nombre d'APS"},
{"var":"NB_APS_MOINS15","role":"integer_count","nlev":0,"desc":"Nombre d'APS débutées avant 15 ans","batt":"Pratique et nombre d'APS"},
{"var":"DEB_15_19","role":"factor_binary","nlev":3,"desc":"Au moins une APS débutée entre 15 et 19 ans","batt":"Pratique et nombre d'APS"},
{"var":"NB_APS_15_19","role":"integer_count","nlev":0,"desc":"Nombre d'APS débutées entre 15 et 19 ans","batt":"Pratique et nombre d'APS"}
]
```

Output:

```json
[
{"level":3,"title":"Pratique selon le lieu, la compagnie et l'âge de début","from":"PRAT_VILLE","to":"NB_APS_15_19"},
{"level":4,"title":"Pratique et nombre d'APS par lieu","from":"PRAT_VILLE","to":"NB_APS_DOMICILE","battery":true},
{"level":4,"title":"Pratique et nombre d'APS selon la compagnie","from":"AVEC_AMIS","to":"NB_APS_SEUL","battery":true},
{"level":4,"title":"Pratique et nombre d'APS selon l'âge de début","from":"DEB_MOINS15","to":"NB_APS_15_19","battery":true}
]
```

### Example 4 — MERGE, EXTEND and BRIDGE a mis-typed member into one battery

All six answer the same question ("… au cours des 4 dernières semaines ?"). The seed tagged only the last three and missed `LIVRE`, an `integer_count` among yes/no items. Make **one** battery — extend back and **bridge `LIVRE`** — and it fills the whole chapter.

Input:

```json
[
{"section":"## Loisirs et pratiques culturelles"},
{"var":"CONCERT","role":"factor_binary","nlev":2,"desc":"Aller à un concert, au cours des 4 dernières semaines","batt":null},
{"var":"CINEMA","role":"factor_binary","nlev":2,"desc":"Aller au cinéma, au cours des 4 dernières semaines","batt":null},
{"var":"LIVRE","role":"integer_count","nlev":0,"desc":"Lire un livre (roman, essai, etc.), au cours des 4 dernières semaines","batt":null},
{"var":"THEATRE_MUSEE_EXPO","role":"factor_binary","nlev":2,"desc":"Aller au théâtre, au musée, voir une exposition, au cours des 4 dernières semaines","batt":"Aller au théâtre"},
{"var":"BD_MANGA","role":"factor_binary","nlev":2,"desc":"Lire une bande dessinée, un manga, au cours des 4 dernières semaines","batt":"Aller au théâtre"},
{"var":"MAGAZINE_REVUE","role":"factor_binary","nlev":2,"desc":"Lire un magazine, une revue, au cours des 4 dernières semaines","batt":"Aller au théâtre"}
]
```

Output:

```json
[
{"level":3,"title":"Loisirs et pratiques culturelles","from":"CONCERT","to":"MAGAZINE_REVUE"},
{"level":4,"title":"Activités culturelles au cours des 4 dernières semaines","from":"CONCERT","to":"MAGAZINE_REVUE","battery":true}
]
```

### Example 5 — distinct nomenclatures = three batteries; leftover questions become a thematic group (nothing loose)

`UNIV_`/`FAM_`/`PAP_` are **three different questions** → three batteries under one chapter (do NOT merge them). Bridge the mis-typed `PAP_AUTRE_GLISSE`. The three remaining standalone questions are **not** a battery, but they still get a thematic `####` group in a second chapter — never left loose.

Input:

```json
[
{"section":"## Répertoire des activités pratiquées"},
{"var":"UNIV_AQUA_NAUT","role":"factor_binary","nlev":2,"desc":"Univers : activités aquatiques et nautiques","batt":"UNIV"},
{"var":"UNIV_SP_COLLECTIFS","role":"factor_binary","nlev":2,"desc":"Univers : sports collectifs","batt":"UNIV"},
{"var":"UNIV_MARCHE_BALADE","role":"factor_binary","nlev":2,"desc":"Univers : marche, balade, course","batt":"UNIV"},
{"var":"FAM_ATHLE","role":"factor_binary","nlev":2,"desc":"Famille : athlétisme","batt":"FAM"},
{"var":"FAM_BASKET","role":"factor_binary","nlev":2,"desc":"Famille : basket","batt":"FAM"},
{"var":"FAM_FOOT","role":"factor_binary","nlev":2,"desc":"Famille : football","batt":"FAM"},
{"var":"PAP_TENNIS","role":"factor_binary","nlev":2,"desc":"Activité détaillée : tennis","batt":"PAP"},
{"var":"PAP_AUTRE_GLISSE","role":"factor_unique_value","nlev":1,"desc":"Activité détaillée : un autre sport de glisse","batt":"PAP"},
{"var":"PAP_JUDO","role":"factor_binary","nlev":2,"desc":"Activité détaillée : judo et déclinaisons","batt":"PAP"},
{"var":"ENVIE_PRAT","role":"factor_binary","nlev":4,"desc":"Envie de pratiquer une APS","batt":null},
{"var":"PRAT_PASSE","role":"factor_binary","nlev":4,"desc":"Pratique d'une APS dans le passé","batt":null},
{"var":"PRAT_FUTUR","role":"factor_binary","nlev":4,"desc":"Intention de pratiquer une APS à l'avenir","batt":null}
]
```

Output:

```json
[
{"level":3,"title":"Répertoire des activités pratiquées","from":"UNIV_AQUA_NAUT","to":"PAP_JUDO"},
{"level":4,"title":"Univers de pratique sportive","from":"UNIV_AQUA_NAUT","to":"UNIV_MARCHE_BALADE","battery":true},
{"level":4,"title":"Familles d'activités pratiquées","from":"FAM_ATHLE","to":"FAM_FOOT","battery":true},
{"level":4,"title":"Activités détaillées pratiquées","from":"PAP_TENNIS","to":"PAP_JUDO","battery":true},
{"level":3,"title":"Rapport à la pratique dans le temps","from":"ENVIE_PRAT","to":"PRAT_FUTUR"},
{"level":4,"title":"Envie, pratique passée et intentions","from":"ENVIE_PRAT","to":"PRAT_FUTUR","battery":false}
]
```

### Example 6 — no batteries: group scattered standalone questions, do NOT emit singletons

Five distinct questions, no battery among them. Gather them into a **couple of coherent thematic groups** — not five one-variable `####`s. A group of two (état de santé) is acceptable.

Input:

```json
[
{"section":"## Habitudes de vie et santé"},
{"var":"TABAC","role":"factor_ordinal","nlev":4,"desc":"Consommation de tabac","batt":null},
{"var":"ALCOOL","role":"factor_ordinal","nlev":5,"desc":"Fréquence de consommation d'alcool","batt":null},
{"var":"SOMMEIL","role":"factor_ordinal","nlev":5,"desc":"Qualité du sommeil","batt":null},
{"var":"IMC","role":"double","nlev":0,"desc":"Indice de masse corporelle","batt":null},
{"var":"SANTE_PERCUE","role":"factor_ordinal","nlev":5,"desc":"État de santé perçu","batt":null}
]
```

Output:

```json
[
{"level":3,"title":"Habitudes de vie et santé","from":"TABAC","to":"SANTE_PERCUE"},
{"level":4,"title":"Habitudes de vie (tabac, alcool, sommeil)","from":"TABAC","to":"SOMMEIL","battery":false},
{"level":4,"title":"État de santé perçu","from":"IMC","to":"SANTE_PERCUE","battery":false}
]
```

---

Return only the JSON array of leveled spans, covering every variable at both levels.
