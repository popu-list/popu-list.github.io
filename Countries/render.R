
library(quarto)
library(tidyverse)


P <- read_csv('/Users/lukefischer/Dropbox/The PopuList Repo/Data/P.csv')


countries <- P |> 
  distinct(country) |>
  mutate(country = case_when(
    country == "Czech_Republic" ~ "Czech Republic", 
    country == "United_Kingdom" ~ "United Kingdom", 
    TRUE ~ country)) |> 
  pull(country) |> 
  as.character()


texts <- c(
  # Austria
  "
# Parties represented in parliament: \n\n
  
The Freiheitliche Partei Österreichs (Freedom Party of Austria, FPÖ) was founded in 1956, but is
generally considered as a far-right and populist party from 1986 onward, when Jörg Haider became
party leader. Under Haider, who left the party in 2005, the party adopted more xenophobic positions
and intensified its populist rhetoric. In the past decades, the FPÖ has experienced varying levels of
electoral success and several spells in government, in coalition with the conservative ÖVP.
Parties not/no longer represented in parliament: \n\n

The Bündnis Zukunft Österreich (Alliance for the Future of Austria, BZÖ) split off from the FPÖ in
2005. Haider became the first party leader and the BZÖ took over the Freedom Party’s position in
the governing coalition. After Haider's death in 2008, the BZÖ softened its stances on immigration
and toned down its populist rhetoric. The party is therefore no longer coded as far right from 2008
onward. In the 2013 elections the party failed to make it into parliament.\n\n

In 2004, the former leader of the Austrian social democrats in the European Parliament (EP), HansPeter Martin, founded his own party: Liste Hans-Peter Martin (Hans-Peter Martin’s List). It
campaigned based on an anti-corruption, pro-transparency and Eurosceptic platform. While it
generally favoured the principle of European integration, the party focused its criticism explicitly on
the institutional structure of the EU and the national political establishment. As such, the party was
both populist and Eurosceptic. Although Martin’s party gained 2 seats in the EP, it failed to cross the
threshold for the national parliament (with 2.8% of the vote in 2006).\n\n

The Liste Pilz, later named JETZT - Liste Pilz (NOW – Pilz List), is a party with an ambiguous
ideological platform. Founded by prominent former Green Party member Peter Pilz in 2017, the
party gained 4.4% of the vote and 8 seats in the 2017 parliamentary election, which it lost again in
the 2019 election. A highly personalised party, Liste Pilz voiced an eclectic set of demands, including
left-wing anti-austerity policies and criticism of ‘political Islam’ (yet its general stance on sociocultural positions was not far right). Pilz has uttered occasional populist criticisms of economic elites
and politicians. Experts disagree on whether the party should be classified as populist and therefore
we consider it a borderline case.\n\n

In 2013, Team Stronach (TS) entered parliament with 5.7% of the vote. Its founder was the
octogenarian millionaire businessman Frank Stronach, who campaigned on a populist, Eurosceptic,
anti-bureaucracy and anti-tax platform and called for a democratisation of the Austrian political 
system. Unlike its Austrian far right populist counterparts, immigration was not a central issue for
Team Stronach. The party was dissolved in 2017.\n\n
  ",
  # Belgium
  "
### Parties represented in parliament: \n\n
  
The Nieuw-Vlaamse Alliantie (New Flemish Alliance, N-VA) was founded in 2001 by a right-leaning
faction of the Flemish nationalist Volksunie (People’s Union, VU). The party is in favour of a gradual
secession of Flanders from Belgium. Although the party has elitist roots, it has become increasingly
populist over the years – also embracing, at times, a people-centric discourse. When it comes to
cultural issues, the party has moved closer to the far right. We have therefore classified the N-VA as
both borderline populist and far right.\n\n

The Vlaams Belang (Flemish Interest, VB) was founded as the Vlaams Blok (Flemish Block) in 1978
and is one of the oldest populist and far-right parties in Europe. The initial goal of the party was
Flemish independence, but soon after its foundation the party became increasingly anti-immigrant.
In 2004, the party was dissolved after it was condemned for racism and continued as the Flemish
Interest. Other parties in Belgium have established a cordon sanitaire against the VB – i.e., they
refuse to cooperate with it in any way or at any level of government.\n\n

The Partij van de Arbeid van België/Parti du Travail de Belgique (Workers' Party of Belgium,
PVDA/PTB) is a Marxist party, founded in 1979. The party is the only Belgian party in parliament
(other parties in Belgium are either Flemish or Wallonian). The PVDA/PTB was a rather small party
until the mid-2010s. From then onwards, the party increased its popularity, in particular in Wallonia.
The PVDA/PTB is far left because of its adoption of radical Marxist principles, and also moderately
Eurosceptic. Although the party does incorporate some populist elements in its rhetoric, overall it
does not have a populist conception of politics. As such we have not classified this party as populist
or borderline populist.\n\n

### Parties not/no longer represented in parliament:\n\n

The Front National (National Front, FN), the Walloon namesake of the French Front National, was
founded in 1985 and propagated a strong unitary Belgian nationalism. The party borrowed many
ideas of Jean-Marie Le Pen’s party with the same name in France. Between 1995 and 2007, the party
received about 2% of the vote. After 2010, the party disappeared from the Chamber of
Representatives.In 2012 the party changed its name to Démocratie Nationale (National Democratie,
DN).\n\n

The populist Lijst Dedecker (List Dedecker, LDD) entered the political stage with 4% of the vote in
2007. This was a neoliberal party, highly critical of the established political order and the EU. The
subsequent elections in 2010 led to a big electoral loss of 4 out of 5 seats. The party has not been
represented in parliament since 2014.\n\n

The populist and far-right Parti Populaire (People’s Party, PP), founded by Rudy Aernoudt and
Mischaël Modrikamen, was active between 2009-2019. This party never received more than 1.5% of
the vote, but it did obtain a parliamentary seat in 2010 and 2014. The party does no longer exist.
  ",
  # Bulgaria
  "
### Parties represented in parliament:
\n\n
Grazhdani za Evropeĭsko Razvitie na Bŭlgariya (Citizens for European Development of Bulgaria,
GERB) is a right-wing populist party founded in 2006 by former National Movement Simeon
II/National Movement for Stability and Progress (NDSV) member and mayor of Sofia Boyko Borisov.
GERB is a conservative and pro-EU force self-professedly based on European Christian Democratic
values. Borisov rose to prominence as the head of a highly personalistic venture, emphasising the
interests of ordinary people (and the ability to solve their problems) and criticising party-political as
well as other elites. The party presents itself as reformist and economically liberal. GERB has been a
central force in Bulgarian politics since the late 2000s and Borisov served three times as prime
minister: between 2009-2013, 2014-2017, and 2017-2021. The party suffered internal splits and a
decline in support after its last stint in government, which was characterised by prolonged protests
against endemic corruption and state capture. In the three elections that followed in 2021, GERB
obtained respectively 26.2%, 23.5% , and 22.4% of the vote amid declining turnout. Borisov’s party
regained some ground and ended up first in the 2022 early election, scoring 24.5% of the vote.
\n\n
Vazrazhdane (Revival) is a populist far-right party founded in 2014 by former Bulgarian National
Movement (VMRO) and National Front for the Salvation of Bulgaria (NFSB) member Kostadin
Kostadinov. The party is fiercely pro-Russian and Eurosceptic with strong anti-Roma, antiimmigration, and anti-LGBTQI+ views. Revival remained at the margins of the Bulgarian electoral
landscape until the November 2021 snap election. , when it crossed the electoral threshold securing
4.9% of the vote. In the 2022 early election, Revival ranked fourth with 9.8% of votes. In this
occasion, the party was largely seen to have capitalised on its pro-Russian stance amid the Russian
invasion of Ukraine and ensuing cost-of-living crisis.
\n\n
### Parties not/no longer represented in parliament:
\n\n
Ataka (Attack) is a populist far-right party founded in 2005 by former TV host Volen Siderov. The
party platform centres on anti-minority, anti-corruption, pro-Russian, and anti-Western views
(hereby including Euroscepticism). Ataka has been one of the most successful far-right parties in the
recent history of Bulgaria. returning 8.1% and 9.4% of the vote, respectively in 2005 and 2009.
Siderov notably made it to the second round of the presidential election in 2006. The party offered
external support to the GERB-led coalition government in 2009 and then acted as kingmaker of the
largely technocratic government led by the Bulgarian Socialist Party and the ethno-liberal Movement
for Rights and Freedoms in 2013. Amid internal scandals and infights, the party’s support has
decreased over time. However, in 2017, Ataka ran as part of the United Patriots (OP) alliance,
including the Bulgarian National Movement (VMRO) and National Front for the Salvation of Bulgaria
(NFSB), which scored 9.1% of the vote and joined the Borisov III government (2017-2021). In 2019,
Ataka bitterly left the far-right alliance and the government altogether. Party support has
plummeted ever since, as also testified by the abysmal results in the four elections held between
2021 and 2022.
\n\n
Izpravi se.BG (Stand Up.BG, ISBG) is a populist anti-establishment and anti-corruption party which
has run as part of the coalition named Stand Up! Mafia, Get Out! (ISMV, until July 2021) and Stand
Up.BG! We Are Coming! (IBG-NI, since July 2021). The coalition included reformist, liberal, and proEU actors. The coalition was established in February 2021. ISBG came to prominence during the antigovernment protests of 2020-2021. While securing representation in the first two elections of 2021,
the coalition failed to pass the threshold in the snap election of November 2021 (2.3% of the vote).
ISBG ran independently in the 2022 early election, but again failed to pass the electoral threshold
(1.0% of the vote).
\n\n
Ima Takav Narod (There Is Such a People, ITN) is a populist party founded in 2020 and led by singer
and cable TV host Slavi Trifonov. The party is anti-establishment, anti-corruption, social conservative,
and pro-EU. ITN returned 17.4% of the vote in the first election contested in April 2021. As the
second most-voted party, it was offered the mandate to form a government, but refused to do so for
the lack of shared principles among elected parties. In the snap election of July 2021, ITN ranked first
scoring 23.8% of the vote, but still failed to form a government coalition after unsuccessful talks with
the liberal Democratic Bulgaria, anti-corruption Stand Up.BG, and the BSP for Bulgaria alliance led by
the Bulgarian Socialist Party. The party paid for its inconsistency and inexperience, and returned only
9.4% of the vote in the snap election of November 2021. After joining the Petkov I government
(2021-2022), Trifonov’s party announced to leave the cabinet over the decision to lift Bulgaria’s veto
on North Macedonia EU accession talks. Such a decision triggered the 2022 early election, in which
ITN scored 3.7% of the vote and failed to re-enter parliament. 
\n\n
The Natsionalno Dvizhenie za Stabilnost i Vazhod (National Movement for Stability and Progress,
NDSV; formerly known as the Natsionalno Dvizhenie \"Simeon Vtori\", National Movement Simeon
II) is a populist party founded in 2001 by Simeon Saxe-Coburg-Gotha (Simeon II), the last tsar of the
Kingdom of Bulgaria between 1943 and 1946. The NDSV adopts an anti-corruption and reformist
platform. was met with enthusiasm upon establishment and the party returned 42.7% of the vote in
the first election contested in 2001. Simeon II then served as PM of a grand coalition comprising the
right-wing Union of Democratic Forces, the left-wing nationalist Bulgarian Socialist Party (BSP), and
the ethno-liberal Movement for Freedom and Rights (DPS). The party largely failed to deliver on its
promises and more than halved its support in 2005 (19.9%), but still took part in the next governing
coalition alongside the BSP and the DPS. The party did not pass the threshold in 2009 and failed to
re-enter parliament since.
\n\n
The Natsionalen Front za Spasenie na Bŭlgariya (National Front for the Salvation of Bulgaria,
NFSB) is a populist far-right party established by Valery Simeonov as a split from the populist farright Ataka in 2011. It is an anti-establishment, anti-immigration, anti-minority, and Eurosceptic
party. The NFSB endorses government spending and is protectionist in terms of Bulgarian business.
The party ran alongside the populist far-right Bulgarian National Movement (VMRO) as part of the
Patriotic Front in 2014, returning 7.3% of the vote. The NFSB and VMRO teamed up with Ataka for
the United Patriots (OP) electoral alliance, which scored 9.1% of the vote and joined the Borisov III
government (2017-2021). With Ataka leaving the OP in 2019, the party contested the 2021 elections
first in alliance with VMRO and the populist far-right Volya. Come the November 2021 snap election,
the NFSB led another far-right coalition, this time without the abovementioned parties. The party
did not manage parliamentary representation in any of the four elections held between 2021 and
2022.
\n\n
Bŭlgariya Bez Tsenzura (Bulgaria Without Censorship, BBT; after 2017 known as Prezaredi
Bŭlgariya, Reload Bulgaria, PB) was a populist and borderline far-right party founded in 2014 by
Nikolay Barekov. The party was anti-corruption, economically liberal, and Eurosceptic, and
presented opposition to Turkey and the Bulgarian Turkish minority as well as anti-immigration as
secondary features of its ideological profile. The party entered parliament in 2014 returning 5.7% of
the vote after a positive performance in the European Parliament election of the same year. In 2017,
the party changed its name to Reload Bulgaria but did not participate in the general election and did
not re-enter parliament since.
\n\n
Red, Zakonnost i Spravedlivost (Order, Law and Justice, RZS) was a populist far-right party founded
in 2005 by Yane Yanev as a rebranding of the National Association – Bulgarian Agrarian People’s
Union. The party was concerned with radical constitutional reform, anti-corruption, and opposition
to Muslim fundamentalism and Islamisation. The party entered parliament in 2009 with 4.7% of the
vote and offered external support to the Borisov I government (2009-2013). RZS was dissolved in
2013.
\n\n
The VMRO – Bŭlgarsko Natsionalno Dvizhenie (Internal Macedonian Revolutionary Organisation –
Bulgarian National Movement, VMRO) is one of the longest-living populist far-right forces of
Bulgaria. It formed as a cultural association in 1991, registered as a party in 1999, and is led by
Krasimir Karakachanov. VMRO claims direct continuity with the historical organisation of the same
name, which was established in 1893. The party is populist far-right, with a strong pan-Bulgarian,
anti-minority, anti-immigration, social conservative, and Eurosceptic agenda. VMRO frequently
contested elections as part of ephemeral electoral coalitions, but in 2014 formed the Patriotic Front
coalition alongside the National Front for the Salvation of Bulgaria (NFSB), returning 7.3% of the
vote. Karakachanov also stood as presidential candidate for the United Patriots (including VMRO,
NFSB, and Ataka) in 2016, ranking third overall. The United Patriots gained 9.1% of the vote in 2017 
and was asked to join the Borisov III government (2017-2021), in which Karakachanov was Minister
of Defence. In the four early elections held between 2021 and 2022, VMRO first ran alone (3.6%,
April 2021), then in coalition with the NFSB and Volya (3.1%, July 2021), and again on its own (1.1%,
November 2021; 0.8%, October 2022), failing to cross the electoral threshold in each occasion.
\n\n
Volya (Will) is a populist far-right party with an anti-corruption, anti-immigration, anti-Western, and
Eurosceptic agenda. The party, led by Veselin Mareshki, operated under different names until 2016
and remained electorally marginal until 2017., when it entered parliament with 4.1% of the vote.
Volya provided support to the Borisov III government (2017-2021), comprising the populist rightwing GERB and the populist far-right United Patriots coalition. In 2021, Volya first contested
elections in alliance with the National Front for the Salvation of Bulgaria and the Bulgarian National
Movement, and then on its own, failing to return any seats in each of the three occasions.
  ",
  # Croatia
  "
### Parties represented in parliament:
  \n\n
The Domovinski Pokret (Homeland Movement, DP) is a populist far-right party founded in 2020 by
singer, former Christian Democratic Union MP, and presidential candidate Miroslav Škoro. The party
is nationalist, social conservative, economically protectionist and critical of the process of
privatisation, yet fully supportive of private property, anti-corruption, and Eurosceptic. The party led
an alliance with the far-right platform Croatian Sovereigntists (including the Croatian Conservative
Party, Croatian Growth, and the Bloc for Croatia) in the 2020 election, returning 10.9% of the vote.
The Croatian Sovereigntists however left the alliance shortly after over internal disputes. The party
has been marred by defections and Škoro himself left the DP in 2021.
\n\n
The Hrvatska Demokratska Zajednica (Christian Democratic Union, HDZ) is a former populist, farright, and Eurosceptic party founded in 1989 by former president of Croatia Franjo Tuđman. During
Tuđman’s tenure at the helm of the party, the HDZ qualified as nativist, anti-Serbian, anticommunist, social conservative, pro-market, and Eurosceptic. The party was the main driver behind
Croatia’s independence in 1991 and responsible for the process of privatisation in the country after
the dissolution of Yugoslavia. The HDZ underwent a process of moderation after the death of
Tuđman in 1999 and now qualifies as a conservative and pro-EU force.
\n\n
The Hrvatski Demokratski Savez Slavonije i Baranje (Croatian Democratic Alliance of Slavonia and
Baranja, HDSSB) is a populist and borderline far-right party founded in 2006 by convicted war
criminal Branimir Glavaš following his ousting from the Croatian Democratic Union (HDZ). The party
is ethno-regionalist, anti-corruption, promotes the economic advancement of Slavonja and Baranja,
and defines itself as pro-EU. The HDSSB is a relatively marginal force and delivered 1 MP as part of
the coalition led by the HDZ in 2020 (which scored 37.26% of the vote). The party has provided
support to the government led by the HDZ since 2020.
\n\n
Hrvatski Suverenisti (Croatian Sovereigntists, HS) is a populist far-right platform launched in 2019,
which has operated as a fully fledged party since 2021. The platform originally comprised the farright Croatian Conservative Party (HKS), Croatian Growth (Hrast), and the Bloc for Croatia (BzH),
which later merged into the HS. The party is nationalist, social conservative, economically
protectionist, and Eurosceptic. Before formalising its transition into a party, HS members teamed up
with Miroslav Škoro’s Homeland Movement (DP) in the 2020 election, collectively returning 10.9% of
the vote and delivering 4 of the 16 MPs of the electoral alliance. HS terminated cooperation with the
DP shortly after the election.
\n\n
Most (Bridge, formerly known as Bridge of Independent Lists) is a populist and borderline far-right
party founded in 2012 by Božo Petrov. The party brought together members of different civic
initiatives and is anti-establishment, anti-corruption, reformist, economically liberal, fiscally
conservative, and has recently taken social conservative and anti-LGBTQI+ views. Most entered
parliament in 2015 with 13.2% of the vote and supported the short-lived government led by the
Christian Democratic Union. The party returned 7.4% of the vote in the 2020 election and currently
sits in opposition.
\n\n
Možemo! (We Can!) is a populist far-left party founded in 2019. The platform brings together
several left-libertarian, democratic socialist, environmentalist, and anti-corruption movements and
initiatives. Možemo! contested the 2020 election gaining 7% of the vote and attaining parliamentary
representation. The following year, Tomislav Tomašević won the mayorship of Zagreb heading the
Green–Left Coalition.
\n\n
### Parties not/no longer represented in parliament:
\n\n
The Bandić Milan 365 – Stranka Rada i Solidarnosti (Bandić Milan 365 – Labour and Solidarity
Party, BM365) is a populist centrist party founded in 2015 by long-serving Zagreb mayor Milan
Bandić. The party is anti-establishment, committed to social justice and increasing jobs, upholding a
critical outlook on the process of transition and privatisation that took place in the country. BM365
managed 4.0% of the vote in the 2016 election as part of a coalition list also including the People’s
Party – Reformists. BM365 did not enter parliament in 2020. Its leader Bandić died in 2021.
\n\n
The Hrvatska Čista Stranka Prava (Croatian Pure Party of Rights, HČSP) is a marginal far-right party
founded in 1992 and claiming affiliation with the party of the same name established in 1904, itself
split from the historical Party of Rights. The party is ideologically extremist, anti-minority, social
conservative, supporting the legacy of the Ustaše and Ante Pavelić, and anti-NATO and hard
Eurosceptic. The HČSP never managed parliamentary representation and peaked at 2.8% of the vote
in 2011, when it contested elections in alliance with the far-right Croatian Party of Rights Dr. Ante
Starčević.
\n\n
The Hrvatska Laburisti-Stranka Rada (Croatian Labourists-Labour Party, HL-SR) is a populist party
founded in 2010 by Dragutin Lesar. The party is social democratic, progressive, and pro-EU. The HLSR entered parliament in 2011 with 5.2% of the vote and joined the coalition Croatia Is Growing led
by the Social Democratic Party, which returned 33.2% of the vote in the 2015 election. The decision
to join the alliance spurred internal dissent and the party subsequently fell into electoral marginality.
The Hrvatska Stranka Prava (Croatian Party of Rights, HSP) is a far-right party founded in 1990 and
claiming historical affiliation with the Party of Rights established in 1861. The party is ideologically
extremist, anti-minority, anti-establishment, social conservative, environmentalist, and Eurosceptic.
\n\n
The HSP peaked at 7.1% of the vote in the 1992 election but failed to re-enter parliament after 2011.
The Hrvatska Stranka Prava Dr. Ante Starčević (Croatian Party of Rights Dr. Ante Starčević, HSP AS)
was a marginal far-right party founded in 2009 by Ruža Tomašić as a splinter from the far-right
Croatian Party of Rights. The party was ideologically extremist, anti-minority, social conservative,
and Eurosceptic. The HSP AS delivered 1 MP (Tomašić herself) in 2011 as part of a coalition with the
far-right Croatian Pure Party of Rights and 3 MPs in 2015 as part of an alliance with the right-wing
Christian Democratic Union. Tomašić left the party in 2014 and joined the Croatian Conservative
Party shortly after. The HSP AS eventually dissolved in 2020.
\n\n
Živi Zid (Human Shield, ŽZ) is a populist party founded in 2011 and led by Ivan Vilibor Sinčić. The
party is ideologically ambiguous. It has anti-establishment, anti-corruption, anti-eviction, libertarian,
economically protectionist, anti-NATO, and Eurosceptic views, and has taken anti-immigration
positions over time. The party peaked at 6.2% of the vote in 2015 as part of the Only Option
Coalition but failed to cross the threshold in 2020, when it ran as ‘Enough of Robbery’ returning 2.3%
of the vote.
  ",
  # Cyprus
  "
  ",
  # Czech Republic
  "
  ",
  # Denmark
  "
  ",
  # Estonia
  "
  ",
  # Finland
  "
  ",
  # France
  "
  ",
  # Germany
  "
  ",
  # Greece
  "
  ",
  # Hungary
  "
  ",
  # Iceland
  "
  ",
  # Ireland
  "
  ",
  # Italy
  "
  ",
  # Latvia
  "
  ",
  # Lithuania
  "
  ",
  # Luxembourg
  "
  ",
  # Netherlands
  "
  ",
  # Norway
  "
  ",
  # Poland
  "
  ",
  # Portugal
  "
  ",
  # Romania
  "
  ",
  # Slovakia
  "
  ",
  # Slovenia
  "
  ",
  # Spain
  "
  ",
  # Sweden
  "
  ",
  # Switzerland
  "
  ",
  # United Kingdom
  "
  "
)


longitudes <- c(
  14.5501,  # Austria
  4.4699,   # Belgium
  25.4858,  # Bulgaria
  15.2000,  # Croatia
  33.4299,  # Cyprus
  15.4730,  # Czech Republic
  9.5018,   # Denmark
  25.0136,  # Estonia
  25.7482,  # Finland
  2.2137,   # France
  10.4515,  # Germany
  21.8243,  # Greece
  19.5033,  # Hungary
  -19.0208, # Iceland
  -8.2439,  # Ireland
  12.5674,  # Italy
  24.6032,  # Latvia
  23.8813,  # Lithuania
  6.1296,   # Luxembourg
  5.2913,   # Netherlands
  8.4689,   # Norway
  19.1451,  # Poland
  -8.2245,  # Portugal
  24.9668,  # Romania
  19.6990,  # Slovakia
  14.9955,  # Slovenia
  -3.7492,  # Spain
  18.6435,  # Sweden
  8.2275,   # Switzerland
  -3.4360   # United Kingdom
)



latitudes <- c(
  47.5162, # Austria
  50.5039, # Belgium
  42.7339, # Bulgaria
  45.1000, # Croatia
  35.1264, # Cyprus
  49.8175, # Czech Republic
  56.2639, # Denmark
  58.5953, # Estonia
  61.9241, # Finland
  46.2276, # France
  51.1657, # Germany
  39.0742, # Greece
  47.1625, # Hungary
  64.9631, # Iceland
  53.4129, # Ireland
  41.8719, # Italy
  56.8796, # Latvia
  55.1694, # Lithuania
  49.8153, # Luxembourg
  52.1326, # Netherlands
  60.4720, # Norway
  51.9194, # Poland
  39.3999, # Portugal
  45.9432, # Romania
  48.6690, # Slovakia
  46.1512, # Slovenia
  40.4637, # Spain
  60.1282, # Sweden
  46.8182, # Switzerland
  55.3781  # United Kingdom
)

iso_codes <- c(
  "AUT", # Austria
  "BEL", # Belgium
  "BGR", # Bulgaria
  "HRV", # Croatia
  "CYP", # Cyprus
  "CZE", # Czech Republic
  "DNK", # Denmark
  "EST", # Estonia
  "FIN", # Finland
  "FRA", # France
  "DEU", # Germany
  "GRC", # Greece
  "HUN", # Hungary
  "ISL", # Iceland
  "IRL", # Ireland
  "ITA", # Italy
  "LVA", # Latvia
  "LTU", # Lithuania
  "LUX", # Luxembourg
  "NLD", # Netherlands
  "NOR", # Norway
  "POL", # Poland
  "PRT", # Portugal
  "ROU", # Romania
  "SVK", # Slovakia
  "SVN", # Slovenia
  "ESP", # Spain
  "SWE", # Sweden
  "CHE", # Switzerland
  "GBR"  # United Kingdom
)


reports <- tibble(
  input = "/Users/lukefischer/Dropbox/The PopuList Repo/Countries/example-report.qmd", 
  output_file = str_glue("{countries}.html"),
  execute_params = pmap(
    list(countries, texts, longitudes, latitudes, iso_codes), 
    ~list(
      country = ..1, 
      text = ..2, 
      longitude = ..3, 
      latitude = ..4,
      iso_code = ..5
    )
  )
)

pwalk(reports, quarto_render)
