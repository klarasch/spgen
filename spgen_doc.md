
Spgen
=====

### Stručný popis programu
Haskell utilita pro generování jednoduchých single-page webových
stránek založených na [Skeleton CSS](http://getskeleton.com).

Uživatel vytvoří jednoduchý textový dokument rozdělený definovanými děliči
na sekce (single-page "řádky") a bloky (single-page "sloupce")
a volitelně "jednoduše naformátovaný" pomocí základních HTML tagů
(ve skutečnosti je však programu jedno, jaké tagy uživatel užije,
stará se pouze o jejich zařazení do stromové struktury layoutu stránky).
Program tento dokument převádí do výsledného HTML souboru kompatibilního
s dodanými CSS soubory založených na boilerplateu Skeleton CSS.

__Uživatelská část dokumentace__ je v anglickém jazyce v souboru `readme.md`.

Programová čast
---------------

### Datové struktury
Program samotný (mimo projektových souborů) má následující strukturu.

```
/src/
|_ Spgen
   |_ Definitions.hs
   |_ Parser.hs
   |_ Writer.hs
   |_ Spgen.hs
|_ Spgen.hs
|_ Main.hs
```

V `Definitions.hs` je definována abstraktní datová struktura dokumentu
pro parsování zdrojového souboru. Dokument se skládá ze sekcí/řádků `Section`,
které mají parametr `param` typu `SecParam` udavající žádané barevné schéma sekce
(`Default` – bílé pozadí, `Dark` – tmavé pozadí, `Color` – barevné pozadí)
a které se dělí na bloky/sloupce `blocks`, které jsou uloženy v poli typu
`[Block]`, přičemž `Block` je `String`. Při případném rozšiřování programu
by bylo možné z typu `Block` udělat složitější typ a strom dále rozvětvit.
Jednotlivé typy bloků (poloviční sloupce, třetinové sloupce...),
v tomto modulu definovány nejsou, protože na úrovni parsování zdrojového
souboru nejsou rozlišovány.

`Parser.hs` obsahuje parsery zdrojového souboru založené na knihovně `Parsec`,
jako parsery pro oddělovače sekcí a bloků, pro sekce, bloky a dokument samotný a další pomocné parsery.

Modul `Writer.hs` slouží k převodu zparsovaných sekcí a bloků na HTML monádu
knihovny `Blaze.Html`. Podle počtu bloků v sekci určí, jestli se má vytvořit
"jumbotron" (velká sekce s jedním výrazným blokem), či jestli mají být bloky
jako sloupce (buď dva nebo tři na řádek). Tyto typy bloků jsou z tohoto důvodu
definovány zde a ne v `Definitions.hs`.

`Spgen.hs` pouze definuje celý modul Spgen (importuje `Definitions`, `Parser`
a `Writer`).

Konečně `Main.hs` se stará o celkový běh programu: načtení a zpracování argumentů
programu z konzole, načtení zdrojového souboru, o spuštění jednotlivých funkcí a o vytvoření výstupního souboru.

### Algoritmy
Program funguje velmi přímočaře.

__Parsování zdrojového programu__ je obstaráváno zejména knihovnou `Parsec`.
Vzhledem k linearitě fungování této knihovny a potřebě jednoúrovně rekurzivní
struktury probíhá parsování ve dvou fázích: nejdříve je zdrojový soubor
na základě jejich oddělovačů rozdělen na sekce v podobě "hrubých" stringů,
a až ve druhé fázi jsou rozparsovány na bloky. Tímto způsobem jsou případné
parsovací chyby správně "zpropagovány". Popis a funkce jednotlivých parserů jsou
popsány v kódu.

__Převod abstraktní struktury na HTML monádu__ je založeno na jednoduchém
"hloubkovém" (moc hlubnoký není :)) průchodu stromem vzniklým při parsování.
V metodě `makeHtmlMonad` jsou do monády vloženy doctype dokuentu a  
na základě argumentů z konzole hlavička; a pro vytvoření těla je zavolána
metoda `astToBody`. Ta prochází pole sekcí a volá na ně metodu `makeSection`.
Ta na základě parametru dané sekce určí třídu "jejího `div`u" (a vytvoří
další pomocné `div`y) a podle počtu bloků v ní rozhoduje, jaké rozvržení
sekce bude mít. Vložení jednotlivých bloků je přímočaré a zřejmé z kódu.

__Hlavní metoda__ je zcela přímočará. O zpracování argumentů se stará knihovna
`CmdArgs`. Jsou načteny argumenty, probíhá kontrola existence zdrojového souboru,
v případě jeho existence je zparsován metodou `parseDoc` z `Parser.hs`, v případě
úspěchu tohoto parsování je dokument převeden na HTML monádu a ta je vykreslena
do výstupního HTML souboru.

Diskuse zpracování
------------------
Program je značně jednodušší, než byl zamýšlen. Zejména, byla
zamýšlena podpora formátování pomocí `markdown`u, ale zjistila jsem, že napsat
si vlastní markdown parser je na trochu delší dobu. Určitě by však bylo
možné přidat "fázi parsování", ve které by se jednotlivé bloky, které jsou
v současném stavu obyčejné stringy, nechaly zparsovat libovolným parserem
`markdown`u.

Na druhou stranu i takto může program (jakkoliv minimálně) ušetřit trochu času
a "otravy" s rozvrhováním struktury HTML dokumentu do struktury boilerplateu
Skeleton.

Závěr
-----
Ačkoliv mě mrzí primitivnost výsledného produktu, myslím si, že i tak jej
někdy využiji, protože často potřebuji vytvořit jednoduchou HTML stránku
této podoby; a dokument dělený dvěma typy čar a s pouze základními tagy je
psát jednodušší a hlavně přehlednější, než celý HTML strom včetně `div`ů
starajících se o layout.

Zároveň si umím představit, že se mi v budoucnu bude hodit si program podle
svých potřeb rozšířit a budu ho možná využívat ještě víe.
