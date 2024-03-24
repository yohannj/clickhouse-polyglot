package com.amendil.entities

// FIXME This list can change between ClickHouse version.
// It'd be great to retrieve it from ClickHouse on startup and use its values for fuzzing.
/**
  * Timezones advertised using `SELECT * FROM system.time_zones`
  */
enum CHTimeZone(val name: String) {
  case Africa_Abidjan extends CHTimeZone("Africa/Abidjan")
  case Africa_Accra extends CHTimeZone("Africa/Accra")
  case Africa_Addis_Ababa extends CHTimeZone("Africa/Addis_Ababa")
  case Africa_Algiers extends CHTimeZone("Africa/Algiers")
  case Africa_Asmara extends CHTimeZone("Africa/Asmara")
  case Africa_Asmera extends CHTimeZone("Africa/Asmera")
  case Africa_Bamako extends CHTimeZone("Africa/Bamako")
  case Africa_Bangui extends CHTimeZone("Africa/Bangui")
  case Africa_Banjul extends CHTimeZone("Africa/Banjul")
  case Africa_Bissau extends CHTimeZone("Africa/Bissau")
  case Africa_Blantyre extends CHTimeZone("Africa/Blantyre")
  case Africa_Brazzaville extends CHTimeZone("Africa/Brazzaville")
  case Africa_Bujumbura extends CHTimeZone("Africa/Bujumbura")
  case Africa_Cairo extends CHTimeZone("Africa/Cairo")
  case Africa_Casablanca extends CHTimeZone("Africa/Casablanca")
  case Africa_Ceuta extends CHTimeZone("Africa/Ceuta")
  case Africa_Conakry extends CHTimeZone("Africa/Conakry")
  case Africa_Dakar extends CHTimeZone("Africa/Dakar")
  case Africa_Dar_es_Salaam extends CHTimeZone("Africa/Dar_es_Salaam")
  case Africa_Djibouti extends CHTimeZone("Africa/Djibouti")
  case Africa_Douala extends CHTimeZone("Africa/Douala")
  case Africa_El_Aaiun extends CHTimeZone("Africa/El_Aaiun")
  case Africa_Freetown extends CHTimeZone("Africa/Freetown")
  case Africa_Gaborone extends CHTimeZone("Africa/Gaborone")
  case Africa_Harare extends CHTimeZone("Africa/Harare")
  case Africa_Johannesburg extends CHTimeZone("Africa/Johannesburg")
  case Africa_Juba extends CHTimeZone("Africa/Juba")
  case Africa_Kampala extends CHTimeZone("Africa/Kampala")
  case Africa_Khartoum extends CHTimeZone("Africa/Khartoum")
  case Africa_Kigali extends CHTimeZone("Africa/Kigali")
  case Africa_Kinshasa extends CHTimeZone("Africa/Kinshasa")
  case Africa_Lagos extends CHTimeZone("Africa/Lagos")
  case Africa_Libreville extends CHTimeZone("Africa/Libreville")
  case Africa_Lome extends CHTimeZone("Africa/Lome")
  case Africa_Luanda extends CHTimeZone("Africa/Luanda")
  case Africa_Lubumbashi extends CHTimeZone("Africa/Lubumbashi")
  case Africa_Lusaka extends CHTimeZone("Africa/Lusaka")
  case Africa_Malabo extends CHTimeZone("Africa/Malabo")
  case Africa_Maputo extends CHTimeZone("Africa/Maputo")
  case Africa_Maseru extends CHTimeZone("Africa/Maseru")
  case Africa_Mbabane extends CHTimeZone("Africa/Mbabane")
  case Africa_Mogadishu extends CHTimeZone("Africa/Mogadishu")
  case Africa_Monrovia extends CHTimeZone("Africa/Monrovia")
  case Africa_Nairobi extends CHTimeZone("Africa/Nairobi")
  case Africa_Ndjamena extends CHTimeZone("Africa/Ndjamena")
  case Africa_Niamey extends CHTimeZone("Africa/Niamey")
  case Africa_Nouakchott extends CHTimeZone("Africa/Nouakchott")
  case Africa_Ouagadougou extends CHTimeZone("Africa/Ouagadougou")
  case Africa_Porto_Novo extends CHTimeZone("Africa/Porto-Novo")
  case Africa_Sao_Tome extends CHTimeZone("Africa/Sao_Tome")
  case Africa_Timbuktu extends CHTimeZone("Africa/Timbuktu")
  case Africa_Tripoli extends CHTimeZone("Africa/Tripoli")
  case Africa_Tunis extends CHTimeZone("Africa/Tunis")
  case Africa_Windhoek extends CHTimeZone("Africa/Windhoek")
  case America_Adak extends CHTimeZone("America/Adak")
  case America_Anchorage extends CHTimeZone("America/Anchorage")
  case America_Anguilla extends CHTimeZone("America/Anguilla")
  case America_Antigua extends CHTimeZone("America/Antigua")
  case America_Araguaina extends CHTimeZone("America/Araguaina")
  case America_Argentina_Buenos_Aires extends CHTimeZone("America/Argentina/Buenos_Aires")
  case America_Argentina_Catamarca extends CHTimeZone("America/Argentina/Catamarca")
  case America_Argentina_ComodRivadavia extends CHTimeZone("America/Argentina/ComodRivadavia")
  case America_Argentina_Cordoba extends CHTimeZone("America/Argentina/Cordoba")
  case America_Argentina_Jujuy extends CHTimeZone("America/Argentina/Jujuy")
  case America_Argentina_La_Rioja extends CHTimeZone("America/Argentina/La_Rioja")
  case America_Argentina_Mendoza extends CHTimeZone("America/Argentina/Mendoza")
  case America_Argentina_Rio_Gallegos extends CHTimeZone("America/Argentina/Rio_Gallegos")
  case America_Argentina_Salta extends CHTimeZone("America/Argentina/Salta")
  case America_Argentina_San_Juan extends CHTimeZone("America/Argentina/San_Juan")
  case America_Argentina_San_Luis extends CHTimeZone("America/Argentina/San_Luis")
  case America_Argentina_Tucuman extends CHTimeZone("America/Argentina/Tucuman")
  case America_Argentina_Ushuaia extends CHTimeZone("America/Argentina/Ushuaia")
  case America_Aruba extends CHTimeZone("America/Aruba")
  case America_Asuncion extends CHTimeZone("America/Asuncion")
  case America_Atikokan extends CHTimeZone("America/Atikokan")
  case America_Atka extends CHTimeZone("America/Atka")
  case America_Bahia extends CHTimeZone("America/Bahia")
  case America_Bahia_Banderas extends CHTimeZone("America/Bahia_Banderas")
  case America_Barbados extends CHTimeZone("America/Barbados")
  case America_Belem extends CHTimeZone("America/Belem")
  case America_Belize extends CHTimeZone("America/Belize")
  case America_Blanc_Sablon extends CHTimeZone("America/Blanc-Sablon")
  case America_Boa_Vista extends CHTimeZone("America/Boa_Vista")
  case America_Bogota extends CHTimeZone("America/Bogota")
  case America_Boise extends CHTimeZone("America/Boise")
  case America_Buenos_Aires extends CHTimeZone("America/Buenos_Aires")
  case America_Cambridge_Bay extends CHTimeZone("America/Cambridge_Bay")
  case America_Campo_Grande extends CHTimeZone("America/Campo_Grande")
  case America_Cancun extends CHTimeZone("America/Cancun")
  case America_Caracas extends CHTimeZone("America/Caracas")
  case America_Catamarca extends CHTimeZone("America/Catamarca")
  case America_Cayenne extends CHTimeZone("America/Cayenne")
  case America_Cayman extends CHTimeZone("America/Cayman")
  case America_Chicago extends CHTimeZone("America/Chicago")
  case America_Chihuahua extends CHTimeZone("America/Chihuahua")
  case America_Ciudad_Juarez extends CHTimeZone("America/Ciudad_Juarez")
  case America_Coral_Harbour extends CHTimeZone("America/Coral_Harbour")
  case America_Cordoba extends CHTimeZone("America/Cordoba")
  case America_Costa_Rica extends CHTimeZone("America/Costa_Rica")
  case America_Creston extends CHTimeZone("America/Creston")
  case America_Cuiaba extends CHTimeZone("America/Cuiaba")
  case America_Curacao extends CHTimeZone("America/Curacao")
  case America_Danmarkshavn extends CHTimeZone("America/Danmarkshavn")
  case America_Dawson extends CHTimeZone("America/Dawson")
  case America_Dawson_Creek extends CHTimeZone("America/Dawson_Creek")
  case America_Denver extends CHTimeZone("America/Denver")
  case America_Detroit extends CHTimeZone("America/Detroit")
  case America_Dominica extends CHTimeZone("America/Dominica")
  case America_Edmonton extends CHTimeZone("America/Edmonton")
  case America_Eirunepe extends CHTimeZone("America/Eirunepe")
  case America_El_Salvador extends CHTimeZone("America/El_Salvador")
  case America_Ensenada extends CHTimeZone("America/Ensenada")
  case America_Fort_Nelson extends CHTimeZone("America/Fort_Nelson")
  case America_Fort_Wayne extends CHTimeZone("America/Fort_Wayne")
  case America_Fortaleza extends CHTimeZone("America/Fortaleza")
  case America_Glace_Bay extends CHTimeZone("America/Glace_Bay")
  case America_Godthab extends CHTimeZone("America/Godthab")
  case America_Goose_Bay extends CHTimeZone("America/Goose_Bay")
  case America_Grand_Turk extends CHTimeZone("America/Grand_Turk")
  case America_Grenada extends CHTimeZone("America/Grenada")
  case America_Guadeloupe extends CHTimeZone("America/Guadeloupe")
  case America_Guatemala extends CHTimeZone("America/Guatemala")
  case America_Guayaquil extends CHTimeZone("America/Guayaquil")
  case America_Guyana extends CHTimeZone("America/Guyana")
  case America_Halifax extends CHTimeZone("America/Halifax")
  case America_Havana extends CHTimeZone("America/Havana")
  case America_Hermosillo extends CHTimeZone("America/Hermosillo")
  case America_Indiana_Indianapolis extends CHTimeZone("America/Indiana/Indianapolis")
  case America_Indiana_Knox extends CHTimeZone("America/Indiana/Knox")
  case America_Indiana_Marengo extends CHTimeZone("America/Indiana/Marengo")
  case America_Indiana_Petersburg extends CHTimeZone("America/Indiana/Petersburg")
  case America_Indiana_Tell_City extends CHTimeZone("America/Indiana/Tell_City")
  case America_Indiana_Vevay extends CHTimeZone("America/Indiana/Vevay")
  case America_Indiana_Vincennes extends CHTimeZone("America/Indiana/Vincennes")
  case America_Indiana_Winamac extends CHTimeZone("America/Indiana/Winamac")
  case America_Indianapolis extends CHTimeZone("America/Indianapolis")
  case America_Inuvik extends CHTimeZone("America/Inuvik")
  case America_Iqaluit extends CHTimeZone("America/Iqaluit")
  case America_Jamaica extends CHTimeZone("America/Jamaica")
  case America_Jujuy extends CHTimeZone("America/Jujuy")
  case America_Juneau extends CHTimeZone("America/Juneau")
  case America_Kentucky_Louisville extends CHTimeZone("America/Kentucky/Louisville")
  case America_Kentucky_Monticello extends CHTimeZone("America/Kentucky/Monticello")
  case America_Knox_IN extends CHTimeZone("America/Knox_IN")
  case America_Kralendijk extends CHTimeZone("America/Kralendijk")
  case America_La_Paz extends CHTimeZone("America/La_Paz")
  case America_Lima extends CHTimeZone("America/Lima")
  case America_Los_Angeles extends CHTimeZone("America/Los_Angeles")
  case America_Louisville extends CHTimeZone("America/Louisville")
  case America_Lower_Princes extends CHTimeZone("America/Lower_Princes")
  case America_Maceio extends CHTimeZone("America/Maceio")
  case America_Managua extends CHTimeZone("America/Managua")
  case America_Manaus extends CHTimeZone("America/Manaus")
  case America_Marigot extends CHTimeZone("America/Marigot")
  case America_Martinique extends CHTimeZone("America/Martinique")
  case America_Matamoros extends CHTimeZone("America/Matamoros")
  case America_Mazatlan extends CHTimeZone("America/Mazatlan")
  case America_Mendoza extends CHTimeZone("America/Mendoza")
  case America_Menominee extends CHTimeZone("America/Menominee")
  case America_Merida extends CHTimeZone("America/Merida")
  case America_Metlakatla extends CHTimeZone("America/Metlakatla")
  case America_Mexico_City extends CHTimeZone("America/Mexico_City")
  case America_Miquelon extends CHTimeZone("America/Miquelon")
  case America_Moncton extends CHTimeZone("America/Moncton")
  case America_Monterrey extends CHTimeZone("America/Monterrey")
  case America_Montevideo extends CHTimeZone("America/Montevideo")
  case America_Montreal extends CHTimeZone("America/Montreal")
  case America_Montserrat extends CHTimeZone("America/Montserrat")
  case America_Nassau extends CHTimeZone("America/Nassau")
  case America_New_York extends CHTimeZone("America/New_York")
  case America_Nipigon extends CHTimeZone("America/Nipigon")
  case America_Nome extends CHTimeZone("America/Nome")
  case America_Noronha extends CHTimeZone("America/Noronha")
  case America_North_Dakota_Beulah extends CHTimeZone("America/North_Dakota/Beulah")
  case America_North_Dakota_Center extends CHTimeZone("America/North_Dakota/Center")
  case America_North_Dakota_New_Salem extends CHTimeZone("America/North_Dakota/New_Salem")
  case America_Nuuk extends CHTimeZone("America/Nuuk")
  case America_Ojinaga extends CHTimeZone("America/Ojinaga")
  case America_Panama extends CHTimeZone("America/Panama")
  case America_Pangnirtung extends CHTimeZone("America/Pangnirtung")
  case America_Paramaribo extends CHTimeZone("America/Paramaribo")
  case America_Phoenix extends CHTimeZone("America/Phoenix")
  case America_Port_au_Prince extends CHTimeZone("America/Port-au-Prince")
  case America_Port_of_Spain extends CHTimeZone("America/Port_of_Spain")
  case America_Porto_Acre extends CHTimeZone("America/Porto_Acre")
  case America_Porto_Velho extends CHTimeZone("America/Porto_Velho")
  case America_Puerto_Rico extends CHTimeZone("America/Puerto_Rico")
  case America_Punta_Arenas extends CHTimeZone("America/Punta_Arenas")
  case America_Rainy_River extends CHTimeZone("America/Rainy_River")
  case America_Rankin_Inlet extends CHTimeZone("America/Rankin_Inlet")
  case America_Recife extends CHTimeZone("America/Recife")
  case America_Regina extends CHTimeZone("America/Regina")
  case America_Resolute extends CHTimeZone("America/Resolute")
  case America_Rio_Branco extends CHTimeZone("America/Rio_Branco")
  case America_Rosario extends CHTimeZone("America/Rosario")
  case America_Santa_Isabel extends CHTimeZone("America/Santa_Isabel")
  case America_Santarem extends CHTimeZone("America/Santarem")
  case America_Santiago extends CHTimeZone("America/Santiago")
  case America_Santo_Domingo extends CHTimeZone("America/Santo_Domingo")
  case America_Sao_Paulo extends CHTimeZone("America/Sao_Paulo")
  case America_Scoresbysund extends CHTimeZone("America/Scoresbysund")
  case America_Shiprock extends CHTimeZone("America/Shiprock")
  case America_Sitka extends CHTimeZone("America/Sitka")
  case America_St_Barthelemy extends CHTimeZone("America/St_Barthelemy")
  case America_St_Johns extends CHTimeZone("America/St_Johns")
  case America_St_Kitts extends CHTimeZone("America/St_Kitts")
  case America_St_Lucia extends CHTimeZone("America/St_Lucia")
  case America_St_Thomas extends CHTimeZone("America/St_Thomas")
  case America_St_Vincent extends CHTimeZone("America/St_Vincent")
  case America_Swift_Current extends CHTimeZone("America/Swift_Current")
  case America_Tegucigalpa extends CHTimeZone("America/Tegucigalpa")
  case America_Thule extends CHTimeZone("America/Thule")
  case America_Thunder_Bay extends CHTimeZone("America/Thunder_Bay")
  case America_Tijuana extends CHTimeZone("America/Tijuana")
  case America_Toronto extends CHTimeZone("America/Toronto")
  case America_Tortola extends CHTimeZone("America/Tortola")
  case America_Vancouver extends CHTimeZone("America/Vancouver")
  case America_Virgin extends CHTimeZone("America/Virgin")
  case America_Whitehorse extends CHTimeZone("America/Whitehorse")
  case America_Winnipeg extends CHTimeZone("America/Winnipeg")
  case America_Yakutat extends CHTimeZone("America/Yakutat")
  case America_Yellowknife extends CHTimeZone("America/Yellowknife")
  case Antarctica_Casey extends CHTimeZone("Antarctica/Casey")
  case Antarctica_Davis extends CHTimeZone("Antarctica/Davis")
  case Antarctica_DumontDUrville extends CHTimeZone("Antarctica/DumontDUrville")
  case Antarctica_Macquarie extends CHTimeZone("Antarctica/Macquarie")
  case Antarctica_Mawson extends CHTimeZone("Antarctica/Mawson")
  case Antarctica_McMurdo extends CHTimeZone("Antarctica/McMurdo")
  case Antarctica_Palmer extends CHTimeZone("Antarctica/Palmer")
  case Antarctica_Rothera extends CHTimeZone("Antarctica/Rothera")
  case Antarctica_South_Pole extends CHTimeZone("Antarctica/South_Pole")
  case Antarctica_Syowa extends CHTimeZone("Antarctica/Syowa")
  case Antarctica_Troll extends CHTimeZone("Antarctica/Troll")
  case Antarctica_Vostok extends CHTimeZone("Antarctica/Vostok")
  case Arctic_Longyearbyen extends CHTimeZone("Arctic/Longyearbyen")
  case Asia_Aden extends CHTimeZone("Asia/Aden")
  case Asia_Almaty extends CHTimeZone("Asia/Almaty")
  case Asia_Amman extends CHTimeZone("Asia/Amman")
  case Asia_Anadyr extends CHTimeZone("Asia/Anadyr")
  case Asia_Aqtau extends CHTimeZone("Asia/Aqtau")
  case Asia_Aqtobe extends CHTimeZone("Asia/Aqtobe")
  case Asia_Ashgabat extends CHTimeZone("Asia/Ashgabat")
  case Asia_Ashkhabad extends CHTimeZone("Asia/Ashkhabad")
  case Asia_Atyrau extends CHTimeZone("Asia/Atyrau")
  case Asia_Baghdad extends CHTimeZone("Asia/Baghdad")
  case Asia_Bahrain extends CHTimeZone("Asia/Bahrain")
  case Asia_Baku extends CHTimeZone("Asia/Baku")
  case Asia_Bangkok extends CHTimeZone("Asia/Bangkok")
  case Asia_Barnaul extends CHTimeZone("Asia/Barnaul")
  case Asia_Beirut extends CHTimeZone("Asia/Beirut")
  case Asia_Bishkek extends CHTimeZone("Asia/Bishkek")
  case Asia_Brunei extends CHTimeZone("Asia/Brunei")
  case Asia_Calcutta extends CHTimeZone("Asia/Calcutta")
  case Asia_Chita extends CHTimeZone("Asia/Chita")
  case Asia_Choibalsan extends CHTimeZone("Asia/Choibalsan")
  case Asia_Chongqing extends CHTimeZone("Asia/Chongqing")
  case Asia_Chungking extends CHTimeZone("Asia/Chungking")
  case Asia_Colombo extends CHTimeZone("Asia/Colombo")
  case Asia_Dacca extends CHTimeZone("Asia/Dacca")
  case Asia_Damascus extends CHTimeZone("Asia/Damascus")
  case Asia_Dhaka extends CHTimeZone("Asia/Dhaka")
  case Asia_Dili extends CHTimeZone("Asia/Dili")
  case Asia_Dubai extends CHTimeZone("Asia/Dubai")
  case Asia_Dushanbe extends CHTimeZone("Asia/Dushanbe")
  case Asia_Famagusta extends CHTimeZone("Asia/Famagusta")
  case Asia_Gaza extends CHTimeZone("Asia/Gaza")
  case Asia_Harbin extends CHTimeZone("Asia/Harbin")
  case Asia_Hebron extends CHTimeZone("Asia/Hebron")
  case Asia_Ho_Chi_Minh extends CHTimeZone("Asia/Ho_Chi_Minh")
  case Asia_Hong_Kong extends CHTimeZone("Asia/Hong_Kong")
  case Asia_Hovd extends CHTimeZone("Asia/Hovd")
  case Asia_Irkutsk extends CHTimeZone("Asia/Irkutsk")
  case Asia_Istanbul extends CHTimeZone("Asia/Istanbul")
  case Asia_Jakarta extends CHTimeZone("Asia/Jakarta")
  case Asia_Jayapura extends CHTimeZone("Asia/Jayapura")
  case Asia_Jerusalem extends CHTimeZone("Asia/Jerusalem")
  case Asia_Kabul extends CHTimeZone("Asia/Kabul")
  case Asia_Kamchatka extends CHTimeZone("Asia/Kamchatka")
  case Asia_Karachi extends CHTimeZone("Asia/Karachi")
  case Asia_Kashgar extends CHTimeZone("Asia/Kashgar")
  case Asia_Kathmandu extends CHTimeZone("Asia/Kathmandu")
  case Asia_Katmandu extends CHTimeZone("Asia/Katmandu")
  case Asia_Khandyga extends CHTimeZone("Asia/Khandyga")
  case Asia_Kolkata extends CHTimeZone("Asia/Kolkata")
  case Asia_Krasnoyarsk extends CHTimeZone("Asia/Krasnoyarsk")
  case Asia_Kuala_Lumpur extends CHTimeZone("Asia/Kuala_Lumpur")
  case Asia_Kuching extends CHTimeZone("Asia/Kuching")
  case Asia_Kuwait extends CHTimeZone("Asia/Kuwait")
  case Asia_Macao extends CHTimeZone("Asia/Macao")
  case Asia_Macau extends CHTimeZone("Asia/Macau")
  case Asia_Magadan extends CHTimeZone("Asia/Magadan")
  case Asia_Makassar extends CHTimeZone("Asia/Makassar")
  case Asia_Manila extends CHTimeZone("Asia/Manila")
  case Asia_Muscat extends CHTimeZone("Asia/Muscat")
  case Asia_Nicosia extends CHTimeZone("Asia/Nicosia")
  case Asia_Novokuznetsk extends CHTimeZone("Asia/Novokuznetsk")
  case Asia_Novosibirsk extends CHTimeZone("Asia/Novosibirsk")
  case Asia_Omsk extends CHTimeZone("Asia/Omsk")
  case Asia_Oral extends CHTimeZone("Asia/Oral")
  case Asia_Phnom_Penh extends CHTimeZone("Asia/Phnom_Penh")
  case Asia_Pontianak extends CHTimeZone("Asia/Pontianak")
  case Asia_Pyongyang extends CHTimeZone("Asia/Pyongyang")
  case Asia_Qatar extends CHTimeZone("Asia/Qatar")
  case Asia_Qostanay extends CHTimeZone("Asia/Qostanay")
  case Asia_Qyzylorda extends CHTimeZone("Asia/Qyzylorda")
  case Asia_Rangoon extends CHTimeZone("Asia/Rangoon")
  case Asia_Riyadh extends CHTimeZone("Asia/Riyadh")
  case Asia_Saigon extends CHTimeZone("Asia/Saigon")
  case Asia_Sakhalin extends CHTimeZone("Asia/Sakhalin")
  case Asia_Samarkand extends CHTimeZone("Asia/Samarkand")
  case Asia_Seoul extends CHTimeZone("Asia/Seoul")
  case Asia_Shanghai extends CHTimeZone("Asia/Shanghai")
  case Asia_Singapore extends CHTimeZone("Asia/Singapore")
  case Asia_Srednekolymsk extends CHTimeZone("Asia/Srednekolymsk")
  case Asia_Taipei extends CHTimeZone("Asia/Taipei")
  case Asia_Tashkent extends CHTimeZone("Asia/Tashkent")
  case Asia_Tbilisi extends CHTimeZone("Asia/Tbilisi")
  case Asia_Tehran extends CHTimeZone("Asia/Tehran")
  case Asia_Tel_Aviv extends CHTimeZone("Asia/Tel_Aviv")
  case Asia_Thimbu extends CHTimeZone("Asia/Thimbu")
  case Asia_Thimphu extends CHTimeZone("Asia/Thimphu")
  case Asia_Tokyo extends CHTimeZone("Asia/Tokyo")
  case Asia_Tomsk extends CHTimeZone("Asia/Tomsk")
  case Asia_Ujung_Pandang extends CHTimeZone("Asia/Ujung_Pandang")
  case Asia_Ulaanbaatar extends CHTimeZone("Asia/Ulaanbaatar")
  case Asia_Ulan_Bator extends CHTimeZone("Asia/Ulan_Bator")
  case Asia_Urumqi extends CHTimeZone("Asia/Urumqi")
  case Asia_Ust_Nera extends CHTimeZone("Asia/Ust-Nera")
  case Asia_Vientiane extends CHTimeZone("Asia/Vientiane")
  case Asia_Vladivostok extends CHTimeZone("Asia/Vladivostok")
  case Asia_Yakutsk extends CHTimeZone("Asia/Yakutsk")
  case Asia_Yangon extends CHTimeZone("Asia/Yangon")
  case Asia_Yekaterinburg extends CHTimeZone("Asia/Yekaterinburg")
  case Asia_Yerevan extends CHTimeZone("Asia/Yerevan")
  case Atlantic_Azores extends CHTimeZone("Atlantic/Azores")
  case Atlantic_Bermuda extends CHTimeZone("Atlantic/Bermuda")
  case Atlantic_Canary extends CHTimeZone("Atlantic/Canary")
  case Atlantic_Cape_Verde extends CHTimeZone("Atlantic/Cape_Verde")
  case Atlantic_Faeroe extends CHTimeZone("Atlantic/Faeroe")
  case Atlantic_Faroe extends CHTimeZone("Atlantic/Faroe")
  case Atlantic_Jan_Mayen extends CHTimeZone("Atlantic/Jan_Mayen")
  case Atlantic_Madeira extends CHTimeZone("Atlantic/Madeira")
  case Atlantic_Reykjavik extends CHTimeZone("Atlantic/Reykjavik")
  case Atlantic_South_Georgia extends CHTimeZone("Atlantic/South_Georgia")
  case Atlantic_St_Helena extends CHTimeZone("Atlantic/St_Helena")
  case Atlantic_Stanley extends CHTimeZone("Atlantic/Stanley")
  case Australia_ACT extends CHTimeZone("Australia/ACT")
  case Australia_Adelaide extends CHTimeZone("Australia/Adelaide")
  case Australia_Brisbane extends CHTimeZone("Australia/Brisbane")
  case Australia_Broken_Hill extends CHTimeZone("Australia/Broken_Hill")
  case Australia_Canberra extends CHTimeZone("Australia/Canberra")
  case Australia_Currie extends CHTimeZone("Australia/Currie")
  case Australia_Darwin extends CHTimeZone("Australia/Darwin")
  case Australia_Eucla extends CHTimeZone("Australia/Eucla")
  case Australia_Hobart extends CHTimeZone("Australia/Hobart")
  case Australia_LHI extends CHTimeZone("Australia/LHI")
  case Australia_Lindeman extends CHTimeZone("Australia/Lindeman")
  case Australia_Lord_Howe extends CHTimeZone("Australia/Lord_Howe")
  case Australia_Melbourne extends CHTimeZone("Australia/Melbourne")
  case Australia_North extends CHTimeZone("Australia/North")
  case Australia_NSW extends CHTimeZone("Australia/NSW")
  case Australia_Perth extends CHTimeZone("Australia/Perth")
  case Australia_Queensland extends CHTimeZone("Australia/Queensland")
  case Australia_South extends CHTimeZone("Australia/South")
  case Australia_Sydney extends CHTimeZone("Australia/Sydney")
  case Australia_Tasmania extends CHTimeZone("Australia/Tasmania")
  case Australia_Victoria extends CHTimeZone("Australia/Victoria")
  case Australia_West extends CHTimeZone("Australia/West")
  case Australia_Yancowinna extends CHTimeZone("Australia/Yancowinna")
  case Brazil_Acre extends CHTimeZone("Brazil/Acre")
  case Brazil_DeNoronha extends CHTimeZone("Brazil/DeNoronha")
  case Brazil_East extends CHTimeZone("Brazil/East")
  case Brazil_West extends CHTimeZone("Brazil/West")
  case Canada_Atlantic extends CHTimeZone("Canada/Atlantic")
  case Canada_Central extends CHTimeZone("Canada/Central")
  case Canada_Eastern extends CHTimeZone("Canada/Eastern")
  case Canada_Mountain extends CHTimeZone("Canada/Mountain")
  case Canada_Newfoundland extends CHTimeZone("Canada/Newfoundland")
  case Canada_Pacific extends CHTimeZone("Canada/Pacific")
  case Canada_Saskatchewan extends CHTimeZone("Canada/Saskatchewan")
  case Canada_Yukon extends CHTimeZone("Canada/Yukon")
  case CET extends CHTimeZone("CET")
  case Chile_Continental extends CHTimeZone("Chile/Continental")
  case Chile_EasterIsland extends CHTimeZone("Chile/EasterIsland")
  case CST6CDT extends CHTimeZone("CST6CDT")
  case Cuba extends CHTimeZone("Cuba")
  case EET extends CHTimeZone("EET")
  case Egypt extends CHTimeZone("Egypt")
  case Eire extends CHTimeZone("Eire")
  case EST extends CHTimeZone("EST")
  case EST5EDT extends CHTimeZone("EST5EDT")
  case Etc_GMT extends CHTimeZone("Etc/GMT")
  case Etc_GMT0 extends CHTimeZone("Etc/GMT0")
  case Etc_GMT_minus_0 extends CHTimeZone("Etc/GMT-0")
  case Etc_GMT_minus_1 extends CHTimeZone("Etc/GMT-1")
  case Etc_GMT_minus_10 extends CHTimeZone("Etc/GMT-10")
  case Etc_GMT_minus_11 extends CHTimeZone("Etc/GMT-11")
  case Etc_GMT_minus_12 extends CHTimeZone("Etc/GMT-12")
  case Etc_GMT_minus_13 extends CHTimeZone("Etc/GMT-13")
  case Etc_GMT_minus_14 extends CHTimeZone("Etc/GMT-14")
  case Etc_GMT_minus_2 extends CHTimeZone("Etc/GMT-2")
  case Etc_GMT_minus_3 extends CHTimeZone("Etc/GMT-3")
  case Etc_GMT_minus_4 extends CHTimeZone("Etc/GMT-4")
  case Etc_GMT_minus_5 extends CHTimeZone("Etc/GMT-5")
  case Etc_GMT_minus_6 extends CHTimeZone("Etc/GMT-6")
  case Etc_GMT_minus_7 extends CHTimeZone("Etc/GMT-7")
  case Etc_GMT_minus_8 extends CHTimeZone("Etc/GMT-8")
  case Etc_GMT_minus_9 extends CHTimeZone("Etc/GMT-9")
  case Etc_GMT_plus_0 extends CHTimeZone("Etc/GMT+0")
  case Etc_GMT_plus_1 extends CHTimeZone("Etc/GMT+1")
  case Etc_GMT_plus_10 extends CHTimeZone("Etc/GMT+10")
  case Etc_GMT_plus_11 extends CHTimeZone("Etc/GMT+11")
  case Etc_GMT_plus_12 extends CHTimeZone("Etc/GMT+12")
  case Etc_GMT_plus_2 extends CHTimeZone("Etc/GMT+2")
  case Etc_GMT_plus_3 extends CHTimeZone("Etc/GMT+3")
  case Etc_GMT_plus_4 extends CHTimeZone("Etc/GMT+4")
  case Etc_GMT_plus_5 extends CHTimeZone("Etc/GMT+5")
  case Etc_GMT_plus_6 extends CHTimeZone("Etc/GMT+6")
  case Etc_GMT_plus_7 extends CHTimeZone("Etc/GMT+7")
  case Etc_GMT_plus_8 extends CHTimeZone("Etc/GMT+8")
  case Etc_GMT_plus_9 extends CHTimeZone("Etc/GMT+9")
  case Etc_Greenwich extends CHTimeZone("Etc/Greenwich")
  case Etc_UCT extends CHTimeZone("Etc/UCT")
  case Etc_Universal extends CHTimeZone("Etc/Universal")
  case Etc_UTC extends CHTimeZone("Etc/UTC")
  case Etc_Zulu extends CHTimeZone("Etc/Zulu")
  case Europe_Amsterdam extends CHTimeZone("Europe/Amsterdam")
  case Europe_Andorra extends CHTimeZone("Europe/Andorra")
  case Europe_Astrakhan extends CHTimeZone("Europe/Astrakhan")
  case Europe_Athens extends CHTimeZone("Europe/Athens")
  case Europe_Belfast extends CHTimeZone("Europe/Belfast")
  case Europe_Belgrade extends CHTimeZone("Europe/Belgrade")
  case Europe_Berlin extends CHTimeZone("Europe/Berlin")
  case Europe_Bratislava extends CHTimeZone("Europe/Bratislava")
  case Europe_Brussels extends CHTimeZone("Europe/Brussels")
  case Europe_Bucharest extends CHTimeZone("Europe/Bucharest")
  case Europe_Budapest extends CHTimeZone("Europe/Budapest")
  case Europe_Busingen extends CHTimeZone("Europe/Busingen")
  case Europe_Chisinau extends CHTimeZone("Europe/Chisinau")
  case Europe_Copenhagen extends CHTimeZone("Europe/Copenhagen")
  case Europe_Dublin extends CHTimeZone("Europe/Dublin")
  case Europe_Gibraltar extends CHTimeZone("Europe/Gibraltar")
  case Europe_Guernsey extends CHTimeZone("Europe/Guernsey")
  case Europe_Helsinki extends CHTimeZone("Europe/Helsinki")
  case Europe_Isle_of_Man extends CHTimeZone("Europe/Isle_of_Man")
  case Europe_Istanbul extends CHTimeZone("Europe/Istanbul")
  case Europe_Jersey extends CHTimeZone("Europe/Jersey")
  case Europe_Kaliningrad extends CHTimeZone("Europe/Kaliningrad")
  case Europe_Kiev extends CHTimeZone("Europe/Kiev")
  case Europe_Kirov extends CHTimeZone("Europe/Kirov")
  case Europe_Kyiv extends CHTimeZone("Europe/Kyiv")
  case Europe_Lisbon extends CHTimeZone("Europe/Lisbon")
  case Europe_Ljubljana extends CHTimeZone("Europe/Ljubljana")
  case Europe_London extends CHTimeZone("Europe/London")
  case Europe_Luxembourg extends CHTimeZone("Europe/Luxembourg")
  case Europe_Madrid extends CHTimeZone("Europe/Madrid")
  case Europe_Malta extends CHTimeZone("Europe/Malta")
  case Europe_Mariehamn extends CHTimeZone("Europe/Mariehamn")
  case Europe_Minsk extends CHTimeZone("Europe/Minsk")
  case Europe_Monaco extends CHTimeZone("Europe/Monaco")
  case Europe_Moscow extends CHTimeZone("Europe/Moscow")
  case Europe_Nicosia extends CHTimeZone("Europe/Nicosia")
  case Europe_Oslo extends CHTimeZone("Europe/Oslo")
  case Europe_Paris extends CHTimeZone("Europe/Paris")
  case Europe_Podgorica extends CHTimeZone("Europe/Podgorica")
  case Europe_Prague extends CHTimeZone("Europe/Prague")
  case Europe_Riga extends CHTimeZone("Europe/Riga")
  case Europe_Rome extends CHTimeZone("Europe/Rome")
  case Europe_Samara extends CHTimeZone("Europe/Samara")
  case Europe_San_Marino extends CHTimeZone("Europe/San_Marino")
  case Europe_Sarajevo extends CHTimeZone("Europe/Sarajevo")
  case Europe_Saratov extends CHTimeZone("Europe/Saratov")
  case Europe_Simferopol extends CHTimeZone("Europe/Simferopol")
  case Europe_Skopje extends CHTimeZone("Europe/Skopje")
  case Europe_Sofia extends CHTimeZone("Europe/Sofia")
  case Europe_Stockholm extends CHTimeZone("Europe/Stockholm")
  case Europe_Tallinn extends CHTimeZone("Europe/Tallinn")
  case Europe_Tirane extends CHTimeZone("Europe/Tirane")
  case Europe_Tiraspol extends CHTimeZone("Europe/Tiraspol")
  case Europe_Ulyanovsk extends CHTimeZone("Europe/Ulyanovsk")
  case Europe_Uzhgorod extends CHTimeZone("Europe/Uzhgorod")
  case Europe_Vaduz extends CHTimeZone("Europe/Vaduz")
  case Europe_Vatican extends CHTimeZone("Europe/Vatican")
  case Europe_Vienna extends CHTimeZone("Europe/Vienna")
  case Europe_Vilnius extends CHTimeZone("Europe/Vilnius")
  case Europe_Volgograd extends CHTimeZone("Europe/Volgograd")
  case Europe_Warsaw extends CHTimeZone("Europe/Warsaw")
  case Europe_Zagreb extends CHTimeZone("Europe/Zagreb")
  case Europe_Zaporozhye extends CHTimeZone("Europe/Zaporozhye")
  case Europe_Zurich extends CHTimeZone("Europe/Zurich")
  case Factory extends CHTimeZone("Factory")
  case GB extends CHTimeZone("GB")
  case GB_Eire extends CHTimeZone("GB-Eire")
  case GMT extends CHTimeZone("GMT")
  case GMT0 extends CHTimeZone("GMT0")
  case GMT_minus_0 extends CHTimeZone("GMT-0")
  case GMT_plus_0 extends CHTimeZone("GMT+0")
  case Greenwich extends CHTimeZone("Greenwich")
  case Hongkong extends CHTimeZone("Hongkong")
  case HST extends CHTimeZone("HST")
  case Iceland extends CHTimeZone("Iceland")
  case Indian_Antananarivo extends CHTimeZone("Indian/Antananarivo")
  case Indian_Chagos extends CHTimeZone("Indian/Chagos")
  case Indian_Christmas extends CHTimeZone("Indian/Christmas")
  case Indian_Cocos extends CHTimeZone("Indian/Cocos")
  case Indian_Comoro extends CHTimeZone("Indian/Comoro")
  case Indian_Kerguelen extends CHTimeZone("Indian/Kerguelen")
  case Indian_Mahe extends CHTimeZone("Indian/Mahe")
  case Indian_Maldives extends CHTimeZone("Indian/Maldives")
  case Indian_Mauritius extends CHTimeZone("Indian/Mauritius")
  case Indian_Mayotte extends CHTimeZone("Indian/Mayotte")
  case Indian_Reunion extends CHTimeZone("Indian/Reunion")
  case Iran extends CHTimeZone("Iran")
  case Israel extends CHTimeZone("Israel")
  case Jamaica extends CHTimeZone("Jamaica")
  case Japan extends CHTimeZone("Japan")
  case Kwajalein extends CHTimeZone("Kwajalein")
  case Libya extends CHTimeZone("Libya")
  case MET extends CHTimeZone("MET")
  case Mexico_BajaNorte extends CHTimeZone("Mexico/BajaNorte")
  case Mexico_BajaSur extends CHTimeZone("Mexico/BajaSur")
  case Mexico_General extends CHTimeZone("Mexico/General")
  case MST extends CHTimeZone("MST")
  case MST7MDT extends CHTimeZone("MST7MDT")
  case Navajo extends CHTimeZone("Navajo")
  case NZ extends CHTimeZone("NZ")
  case NZ_CHAT extends CHTimeZone("NZ-CHAT")
  case Pacific_Apia extends CHTimeZone("Pacific/Apia")
  case Pacific_Auckland extends CHTimeZone("Pacific/Auckland")
  case Pacific_Bougainville extends CHTimeZone("Pacific/Bougainville")
  case Pacific_Chatham extends CHTimeZone("Pacific/Chatham")
  case Pacific_Chuuk extends CHTimeZone("Pacific/Chuuk")
  case Pacific_Easter extends CHTimeZone("Pacific/Easter")
  case Pacific_Efate extends CHTimeZone("Pacific/Efate")
  case Pacific_Enderbury extends CHTimeZone("Pacific/Enderbury")
  case Pacific_Fakaofo extends CHTimeZone("Pacific/Fakaofo")
  case Pacific_Fiji extends CHTimeZone("Pacific/Fiji")
  case Pacific_Funafuti extends CHTimeZone("Pacific/Funafuti")
  case Pacific_Galapagos extends CHTimeZone("Pacific/Galapagos")
  case Pacific_Gambier extends CHTimeZone("Pacific/Gambier")
  case Pacific_Guadalcanal extends CHTimeZone("Pacific/Guadalcanal")
  case Pacific_Guam extends CHTimeZone("Pacific/Guam")
  case Pacific_Honolulu extends CHTimeZone("Pacific/Honolulu")
  case Pacific_Johnston extends CHTimeZone("Pacific/Johnston")
  case Pacific_Kanton extends CHTimeZone("Pacific/Kanton")
  case Pacific_Kiritimati extends CHTimeZone("Pacific/Kiritimati")
  case Pacific_Kosrae extends CHTimeZone("Pacific/Kosrae")
  case Pacific_Kwajalein extends CHTimeZone("Pacific/Kwajalein")
  case Pacific_Majuro extends CHTimeZone("Pacific/Majuro")
  case Pacific_Marquesas extends CHTimeZone("Pacific/Marquesas")
  case Pacific_Midway extends CHTimeZone("Pacific/Midway")
  case Pacific_Nauru extends CHTimeZone("Pacific/Nauru")
  case Pacific_Niue extends CHTimeZone("Pacific/Niue")
  case Pacific_Norfolk extends CHTimeZone("Pacific/Norfolk")
  case Pacific_Noumea extends CHTimeZone("Pacific/Noumea")
  case Pacific_Pago_Pago extends CHTimeZone("Pacific/Pago_Pago")
  case Pacific_Palau extends CHTimeZone("Pacific/Palau")
  case Pacific_Pitcairn extends CHTimeZone("Pacific/Pitcairn")
  case Pacific_Pohnpei extends CHTimeZone("Pacific/Pohnpei")
  case Pacific_Ponape extends CHTimeZone("Pacific/Ponape")
  case Pacific_Port_Moresby extends CHTimeZone("Pacific/Port_Moresby")
  case Pacific_Rarotonga extends CHTimeZone("Pacific/Rarotonga")
  case Pacific_Saipan extends CHTimeZone("Pacific/Saipan")
  case Pacific_Samoa extends CHTimeZone("Pacific/Samoa")
  case Pacific_Tahiti extends CHTimeZone("Pacific/Tahiti")
  case Pacific_Tarawa extends CHTimeZone("Pacific/Tarawa")
  case Pacific_Tongatapu extends CHTimeZone("Pacific/Tongatapu")
  case Pacific_Truk extends CHTimeZone("Pacific/Truk")
  case Pacific_Wake extends CHTimeZone("Pacific/Wake")
  case Pacific_Wallis extends CHTimeZone("Pacific/Wallis")
  case Pacific_Yap extends CHTimeZone("Pacific/Yap")
  case Poland extends CHTimeZone("Poland")
  case Portugal extends CHTimeZone("Portugal")
  case PRC extends CHTimeZone("PRC")
  case PST8PDT extends CHTimeZone("PST8PDT")
  case ROC extends CHTimeZone("ROC")
  case ROK extends CHTimeZone("ROK")
  case Singapore extends CHTimeZone("Singapore")
  case Turkey extends CHTimeZone("Turkey")
  case UCT extends CHTimeZone("UCT")
  case Universal extends CHTimeZone("Universal")
  case US_Alaska extends CHTimeZone("US/Alaska")
  case US_Aleutian extends CHTimeZone("US/Aleutian")
  case US_Arizona extends CHTimeZone("US/Arizona")
  case US_Central extends CHTimeZone("US/Central")
  case US_East_Indiana extends CHTimeZone("US/East-Indiana")
  case US_Eastern extends CHTimeZone("US/Eastern")
  case US_Hawaii extends CHTimeZone("US/Hawaii")
  case US_Indiana_Starke extends CHTimeZone("US/Indiana-Starke")
  case US_Michigan extends CHTimeZone("US/Michigan")
  case US_Mountain extends CHTimeZone("US/Mountain")
  case US_Pacific extends CHTimeZone("US/Pacific")
  case US_Samoa extends CHTimeZone("US/Samoa")
  case UTC extends CHTimeZone("UTC")
  case W_SU extends CHTimeZone("W-SU")
  case WET extends CHTimeZone("WET")
  case Zulu extends CHTimeZone("Zulu")
}
