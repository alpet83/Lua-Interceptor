-- СКРИПТ ПРЕДНАЗНАЧЕН ДЛЯ СОКРЫТИЯ В LUAICP.DLL. НЕ РАЗДАВАТЬ ЕГО ТЕСТЕРАМ!
if _G.sak_dialog == nil then
   LuaSafeCall("$game_scripts$\\sak_dialog.script", "#file:^dbg")
end

if _G.hidden == nil then
   _G.hidden = {}   
   setmetatable(hidden, { __index = G }) 
   setfenv(1, hidden)
   wprintf(" x-module 'hidden' registered as %s ", type(hidden))
end

local x_storage = {}

function x_storage.item_add(section, count)
  sak.create_items(db.actor, section, count)
end

function x_storage.item_add_random(section, max_count)
  local rand=lua_random(1, max_count)
  sak.create_items(db.actor, section, rand)
end

function x_storage.item_have(section)
  return sak.have_items_count(section) ~= false
end

function x_storage.item_give(section, count, reward_pts)
  sak_inventory.release_actor_items(section, count)
  if reward_pts then
     xr_statistic.quest_success("st_items_delivered", reward_pts)
  end
end


function x_storage.add_max_nagrad()
  sak.create_items(db.actor,"vodka",1)
  sak.create_items(db.actor,"wpn_eagle_m1",1)
end
function x_storage.add_flesh_leather()
  sak.create_items(db.actor,"flesh_leather",1)
end
function x_storage.flesh_leather_have()
  return sak.have_items_count("flesh_leather",1)~=false
end
function x_storage.give_flesh_leather()
  sak_inventory.release_actor_items("flesh_leather", 1)
end
function x_storage.flesh_2leather_have()
  return sak.have_items_count("flesh_leather",2)~=false
end
function x_storage.give_flesh_2leather()
  sak_inventory.release_actor_items("flesh_leather", 2)
end
function x_storage.flesh_3leather_have()
  return sak.have_items_count("flesh_leather",3)~=false
end
function x_storage.give_flesh_3leather()
  sak_inventory.release_actor_items("flesh_leather", 3)
end
function x_storage.flesh_4leather_have()
  return sak.have_items_count("flesh_leather",4)~=false
end
function x_storage.give_flesh_4leather()
  sak_inventory.release_actor_items("flesh_leather", 4)
end
function x_storage.add_stepanych_1plash()
  sak.create_items(db.actor,"bandit_veteran_outfit",1)
end
function x_storage.add_stepanych_1plash_gas()
  sak.create_items(db.actor,"bandit_veteran_outfit_gas",1)
end
function x_storage.add_stepanych_2plash()
  sak.create_items(db.actor,"bandit_master_outfit",1)
end
function x_storage.add_stepanych_2plash_gas()
  sak.create_items(db.actor,"bandit_master_outfit_gas",1)
end

function x_storage.flesh_shkura_have()
  return sak.have_items_count("mutant_flesh_shkura",1)~=false
end
function x_storage.give_flesh_shkura()
  sak_inventory.release_actor_items("mutant_flesh_shkura", 1)
end
function x_storage.flesh_2shkura_have()
  return sak.have_items_count("mutant_flesh_shkura",2)~=false
end
function x_storage.give_flesh_2shkura()
  sak_inventory.release_actor_items("mutant_flesh_shkura", 2)
end
function x_storage.flesh_4shkura_have()
  return sak.have_items_count("mutant_flesh_shkura",4)~=false
end
function x_storage.give_flesh_4shkura()
  sak_inventory.release_actor_items("mutant_flesh_shkura", 4)
end
function x_storage.add_meshok()
  sak.create_items(db.actor,"solt_bag",1)
end
function x_storage.meshok_have()
  return sak.have_items_count("solt_bag",1)~=false
end
function x_storage.give_meshok()
  sak_inventory.release_actor_items("solt_bag", 1)
end
function x_storage.add_acid_banka()
  sak.create_items(db.actor,"acid_banka",1)
end
function x_storage.acid_banka_have()
  return sak.have_items_count("acid_banka",1)~=false
end
function x_storage.give_acid_banka()
  sak_inventory.release_actor_items("acid_banka", 1)
end
function x_storage.add_cristall_flower()
  local rand=lua_random(1,4)
  sak.create_items(db.actor,"af_cristal_flower_dyn"..rand.."d",1)
end
function x_storage.add_drops()
  local rand=lua_random(1,4)
  sak.create_items(db.actor,"af_drops_dyn"..rand.."d",1)
end
function x_storage.add_blood()
  local rand=lua_random(1,4)
  sak.create_items(db.actor,"af_blood_dyn"..rand.."d",1)
end
function x_storage.add_vyvert()
  local rand=lua_random(1,4)
  sak.create_items(db.actor,"af_vyvert_dyn"..rand.."d",1)
end
function x_storage.take_vyvert()
  local rand=lua_random(1,4)
  sak.create_items(db.actor,"af_vyvert_dyn"..rand.."d",1)
end
function x_storage.add_medusa()
  local rand=lua_random(1,4)
  sak.create_items(db.actor,"af_medusa_dyn"..rand.."d",1)
end
function x_storage.add_gravi()
  local rand=lua_random(1,4)
  sak.create_items(db.actor,"af_gravi_dyn"..rand.."d",1)
end
function x_storage.add_mincer_meat()
  local rand=lua_random(1,4)
  sak.create_items(db.actor,"af_mincer_meat_dyn"..rand.."d",1)
end
function x_storage.add_electra_flash()
  local rand=lua_random(1,4)
  sak.create_items(db.actor,"af_electra_flash_dyn"..rand.."d",1)
end
function x_storage.add_electra_sparkler()
  local rand=lua_random(1,4)
  sak.create_items(db.actor,"af_electra_sparkler_dyn"..rand.."d",1)
end
function x_storage.add_electra_moonlight_capsule()
  local rand=lua_random(1,4)
  sak.create_items(db.actor,"af_electra_moonlight_capsule_dyn"..rand.."d",1)
end
function x_storage.add_fireball()
  local rand=lua_random(1,4)
  sak.create_items(db.actor,"af_fireball_dyn"..rand.."d",1)
end
function x_storage.add_ameba_slug()
  local rand=lua_random(1,4)
  sak.create_items(db.actor,"af_ameba_slug_dyn"..rand.."d",1)
end
function x_storage.add_rusty_kristall()
  local rand=lua_random(1,4)
  sak.create_items(db.actor,"af_rusty_kristall_dyn"..rand.."d",1)
end
function x_storage.add_night_star()
  local rand=lua_random(1,4)
  sak.create_items(db.actor,"af_night_star_dyn"..rand.."d",1)
end
function x_storage.add_gold_fish()
  local rand=lua_random(1,4)
  sak.create_items(db.actor,"af_gold_fish_dyn"..rand.."d",1)
end
function x_storage.add_soul()
  local rand=lua_random(1,4)
  sak.create_items(db.actor,"af_soul_dyn"..rand.."d",1)
end
function x_storage.add_electra_moonlight()
  local rand=lua_random(1,4)
  sak.create_items(db.actor,"af_electra_moonlight_dyn"..rand.."d",1)
end
function x_storage.add_cristall()
  local rand=lua_random(1,4)
  sak.create_items(db.actor,"af_cristall_dyn"..rand.."d",1)
end
function x_storage.add_ameba_mica()
  local rand=lua_random(1,4)
  sak.create_items(db.actor,"af_ameba_mica_dyn"..rand.."d",1)
end
function x_storage.add_rusty_sea_urchin()
  local rand=lua_random(1,4)
  sak.create_items(db.actor,"af_rusty_sea-urchin_dyn"..rand.."d",1)
end
function x_storage.add_dummy_spring()
  sak.create_items(db.actor,"af_dummy_spring",1)
end
function x_storage.add_dummy_dummy()
  sak.create_items(db.actor,"af_dummy_dummy",1)
end
function x_storage.add_dummy_battery()
  sak.create_items(db.actor,"af_dummy_battery",1)
end
function x_storage.add_dummy_pellicle()
  sak.create_items(db.actor,"af_dummy_pellicle",1)
end
function x_storage.add_dummy_glassbeads()
  sak.create_items(db.actor,"af_dummy_glassbeads",1)
end

function x_storage.take_2_ak74()
  sak.create_items(db.actor, "vodka",5)
  --sak.create_items(db.actor, "ammo_super_gauss",30)
  --sak.create_items(db.actor, "af_babka_3",1)
  --sak.create_items(db.actor, "af_spirit_4",2)
  --sak.create_items(db.actor, "dollars5",2)
  --sak.create_items(db.actor, "dollars1",2)
end
function x_storage.take_sakharov_mil_kurjer_1nagrad()
  sak.create_items(db.actor, "ammo_12x70_new",10)
end
function x_storage.take_sakharov_mil_kurjer_2nagrad()
  sak.create_items(db.actor,"medkit_scientic",5)
  sak.create_items(db.actor,"antirad",5)
  sak.create_items(db.actor,"suhpay",5)
end
function x_storage.add_outfit_neumeha()
  local points={
  1.1544072628021,7.4998812675476,18.371746063232,241841,488,
  -28.24542427063,4.9999976158142,-3.1245086193085,231059,682,
  16.459121704102,-1.9259337186813,2.3280971050262,236709,487,
  41.057662963867,2.4978001117706,-16.402126312256,279274,663,
  28.686042785645,4.8176741600037,-32.532073974609,278644,663,
  34.529563903809,9.2246599197388,-20.568384170532,278644,663,
  69.733047485352,0.54690098762512,20.116687774658,305852,671,
  61.037391662598,1.2793383598328,-20.495662689209,304519,671,
  -17.107900619507,0.98500883579254,-11.403326034546,304519,671,
  -11.100729942322,-0.20154552161694,35.173683166504,230399,490
}
  local rand=lua_random(1,10)*5
  local obj=amk.spawn_item("physic_destroyable_object",sak.v3f(points[rand-4],points[rand-3],points[rand-2]),points[rand],points[rand-1])
  if obj then
    local pk = get_netpk(obj,1)
    local data = pk:get()
    data.visual_name = "physics\\box\\box_wood_01.ogf"
    data.custom_data = "[drop_box]\ncommunity = def_box\nitems = stalker_outfit_m3"
    data.mass = 10
    pk:set(data)    
  end
end

function x_storage.add_boloto_dinamit()
  g_sim:create("dynamite",sak.v3f(273.89840698242,9.7082767486572,66.893074035645),311890,3527)
end
function x_storage.add_sanat_dinamit()
  g_sim:create("dynamite",sak.v3f(-97.657897949219,32.699840545654,614.76776123047),550,3038)
end
function x_storage.add_kishka_dinamit()
  g_sim:create("dynamite",sak.v3f(238.81538391113,-3.7692546844482,566.88269042969),679465,4102)
end
function x_storage.add_sidor_dinamit()
  g_sim:create("dynamite",sak.v3f(-164.822265625,-19.815210342407,-129.96162414551),81353,55)
end

function x_storage.add_drunk_abakan()
  local points={
  {-16.97483062744,14.2,46.94319915771,9400,3327},
  {-18.81781578064,14.2,49.63105010986,9005,3327},
  {2.2395524978638,15.7,37.87773895263,14221,3330},
  {82.932456970215,10.2,33.28562927246,38642,3333},
  {81.270751953125,10.2,40.08720397949,38189,3333},
  {76.121879577637,10.1,26.44740104675,36735,3333},
  {64.034172058105,11.5,52.59435272216,32969,3336},
  }
  local rand=lua_random(#points)
  g_sim:create("wpn_abakan_m2",sak.v3f(points[rand][1],points[rand][2],points[rand][3]),points[rand][4],points[rand][5])
end
function x_storage.add_drunk_dolg_treasure()
  local ids={2113,5864}
  local obj1 = g_sim:story_object(ids[lua_random(1,2)])
  if obj1 then
  local item = ""
  local count = lua_random(4,5)
    for i=1,count do
    item = nagrad.level_1[lua_random(table.getn(nagrad.level_1))]
    g_sim:create(item,obj1.position, obj1.m_level_vertex_id, obj1.m_game_vertex_id, obj1.id)
    end
  g_sim:create("drunk_dolg_letter",obj1.position, obj1.m_level_vertex_id, obj1.m_game_vertex_id, obj1.id)
  end
end
function x_storage.add_hunters_treasure1()
  local obj1 = g_sim:story_object(story_ids.dsc_inventory_box_10)
  if obj1 then
  local item = ""
  local count = lua_random(4,5)
    for i=1,count do
    item = nagrad.level_4[lua_random(table.getn(nagrad.level_4))]
    g_sim:create(item,obj1.position, obj1.m_level_vertex_id, obj1.m_game_vertex_id, obj1.id)
    end
  end
end
function x_storage.add_stalker_treasure()
  local obj1 = g_sim:story_object(story_ids.red_treasure_4)
  if obj1 then
    g_sim:create("af_soul_capsule_dyn2d",obj1.position, obj1.m_level_vertex_id, obj1.m_game_vertex_id, obj1.id)
    g_sim:create("af_cristall_capsule_dyn2d", obj1.m_level_vertex_id, obj1.m_game_vertex_id, obj1.id)
    g_sim:create("af_gold_fish_capsule_dyn3d",obj1.position, obj1.m_level_vertex_id, obj1.m_game_vertex_id, obj1.id)
    g_sim:create("af_night_star_capsule_dyn3d",obj1.position, obj1.m_level_vertex_id, obj1.m_game_vertex_id, obj1.id)
    g_sim:create("af_electra_flash_dyn4d",obj1.position, obj1.m_level_vertex_id, obj1.m_game_vertex_id, obj1.id)
  end
end
function x_storage.add_skull_gift()
  local obj1 = g_sim:story_object(story_ids.bar_inventory_box_0006)
  if obj1 then
  local item = ""
  local count = lua_random(1,2)
    for i=1,count do
    item = table_dart[lua_random(table.getn(table_dart))]
    g_sim:create(item,obj1.position, obj1.m_level_vertex_id, obj1.m_game_vertex_id, obj1.id)
    end
  end
end
function x_storage.add_hunters_treasure2()
  local obj1 = g_sim:story_object(story_ids.mar_q_n_13)
  if obj1 then
  local item = ""
  local count = lua_random(4,5)
    for i=1,count do
    item = nagrad.level_3[lua_random(table.getn(nagrad.level_3))]
    g_sim:create(item,obj1.position, obj1.m_level_vertex_id, obj1.m_game_vertex_id, obj1.id)
    end
  end
end
function x_storage.add_hunters_treasure3()
  local obj1 = g_sim:story_object(story_ids.red_treasure_25)
  if obj1 then
  local item = ""
  local count = lua_random(4,5)
    for i=1,count do
    item = nagrad.level_1[lua_random(table.getn(nagrad.level_1))]
    g_sim:create(item,obj1.position, obj1.m_level_vertex_id, obj1.m_game_vertex_id, obj1.id)
    end
  end
end
function x_storage.add_hunters_toz_in1()
  local obj1 = g_sim:story_object(story_ids.dsc_inventory_box_17)
  g_sim:create("wpn_hunters_toz_new",obj1.position, obj1.m_level_vertex_id, obj1.m_game_vertex_id, obj1.id)
  if obj1.online then
    g_sim:set_switch_online(obj1.id, false)
    g_sim:set_switch_offline(obj1.id, true)
    amk.convert_npc[obj1.id]=1
  end
end
function x_storage.add_hunters_toz_in2()
  local obj1 = g_sim:story_object(story_ids.dsc_inventory_box_03)
  g_sim:create("wpn_hunters_toz_new",obj1.position, obj1.m_level_vertex_id, obj1.m_game_vertex_id, obj1.id)
  if obj1.online then
    g_sim:set_switch_online(obj1.id, false)
    g_sim:set_switch_offline(obj1.id, true)
    amk.convert_npc[obj1.id]=1
  end
end
function x_storage.add_hunters_toz_in3()
  local obj1 = g_sim:story_object(story_ids.dsc_inventory_box_05)
  g_sim:create("wpn_hunters_toz_new",obj1.position, obj1.m_level_vertex_id, obj1.m_game_vertex_id, obj1.id)
  if obj1.online then
    g_sim:set_switch_online(obj1.id, false)
    g_sim:set_switch_offline(obj1.id, true)
    amk.convert_npc[obj1.id]=1
  end
end
function x_storage.add_hunters_toz_in4()
  local obj1 = g_sim:story_object(story_ids.dsc_inventory_box_07)
  g_sim:create("wpn_hunters_toz_new",obj1.position, obj1.m_level_vertex_id, obj1.m_game_vertex_id, obj1.id)
  if obj1.online then
    g_sim:set_switch_online(obj1.id, false)
    g_sim:set_switch_offline(obj1.id, true)
    amk.convert_npc[obj1.id]=1
  end
end
function x_storage.fill_agr_u_treasure()
  local obj1 = g_sim:story_object(story_ids.agr_und_inventory_box)
  g_sim:create("redcatcher_albom",obj1.position, obj1.m_level_vertex_id, obj1.m_game_vertex_id, obj1.id)
end
function x_storage.fill_red_terasure()
  local obj1 = g_sim:story_object(story_ids.red_inventory_box_01)
  local rand = lua_random(3,5)
  for a=1,rand do
    local section=table_bart[lua_random(1,#table_bart)]
    local b=lua_random(2,4)
    if not strpos(section, "_new",1,true) and not strpos(section, "_dummy_",1,true) then
    section=section.."_dyn"..b.."d"
    end
    g_sim:create(section,obj1.position, obj1.m_level_vertex_id, obj1.m_game_vertex_id, obj1.id)
  end
  g_sim:create("red_shtreck_letter",obj1.position, obj1.m_level_vertex_id, obj1.m_game_vertex_id, obj1.id)
end
function x_storage.fill_food_prip()
  local rand = lua_random(3,5)
  for a=1,rand do
    g_sim:create("bread",vector():set(-2.82+(a*0.03),-4.05,192.28+(a*0.03)),97081,2195)
  end
  rand = lua_random(3,6)
  for a=1,rand do
    g_sim:create("kolbasa",vector():set(-2.82+(a*0.05),-4.05,193.12+(a*0.03)),97951,2195)
  end
end

function x_storage.dar_document3_have()
  return sak.have_items_count("dar_document3",1) and not sak.have_items_count("dar_document5",1)
end
function x_storage.give_dar_document3()
  sak_inventory.release_actor_items("dar_document3",1)
end
function x_storage.dar_document5_have()
  return sak.have_items_count("dar_document5",1) and not sak.have_items_count("dar_document3",1)
end
function x_storage.give_dar_document5()
  sak_inventory.release_actor_items("dar_document5",1)
end
function x_storage.dar_document35_have()
  return sak.have_items_count("dar_document5",1) and sak.have_items_count("dar_document3",1)
end
function x_storage.give_dar_document35()
  sak_inventory.release_actor_items("dar_document3",1)
  sak_inventory.release_actor_items("dar_document5",1)
end
function x_storage.fill_bar_new_game()
  local obj1 = g_sim:story_object(story_ids.bar_inventory_box)
  g_sim:create("razgruzka",obj1.position, obj1.m_level_vertex_id, obj1.m_game_vertex_id, obj1.id)
  g_sim:create("podsumok",obj1.position, obj1.m_level_vertex_id, obj1.m_game_vertex_id, obj1.id)
end
function x_storage.start_time_ivancov_good()
  timers.start_timer("time_ivancov_done",0,17,15,"sak.ivancov_case_good()")
end
function x_storage.start_time_ivancov_good_done()
  timers.start_timer("time_ivancov_good_done",1,19,15,"sak.ivancov_case_good()")
end
function x_storage.fill_ivancov_treasure()
    local obj1 = g_sim:story_object(story_ids.bar_inventory_box_0018)
    local rand = lua_random(3,5)
  for a=1,rand do
    local section=table_aart[lua_random(1,#table_aart)]
    local b=lua_random(2,4)
    if not strpos(section, "_new",1,true) and not strpos(section, "_dummy_",1,true) then
    section=section.."_dyn"..b.."d"
    end
    g_sim:create(section,obj1.position, obj1.m_level_vertex_id, obj1.m_game_vertex_id, obj1.id)
  end
end
function x_storage.start_time_ivancov_case()
  timers.start_timer("time_ivancov_case",1,1,15,"sak_dialog.ivancov_case_enemy()")
end
function x_storage.stop_time_ivancov_case()
  timers.stop_timer("time_ivancov_case")
end
function x_storage.ivancov_case_enemy()
  local ivancov=level_object_by_sid(505)
  if ivancov then
    ivancov:set_relation(game_object.enemy, db.actor)
  end
  xr_gulag.setGulagEnemy("bar_dolg_general", db.actor)
  xr_gulag.setGulagEnemy("bar_zastava", db.actor)
  xr_gulag.setGulagEnemy("bar_zastava_2", db.actor)
end
function x_storage.dolg_mil_enemy()
  xr_gulag.setGulagEnemy("mil_dolg", db.actor)
end
function x_storage.mil_dolg_payment()
  local art=rand_bart()
  sak.create_items(db.actor,art,1)
  sak_dialog.relocate_money(db.actor, 25000, "in")
end

function x_storage.text_talk_dialog(text_talk)
  db.actor:give_talk_message("%c[255,216,186,140]"..sak_dialog.blowout_when.." "..text_talk,"ui\\ui_iconsTotal",Frect():set(0,0,0,0),"simple_answer_item")
end
function x_storage.give_marsh_tuzla_document()
  sak_inventory.release_actor_items("marsh_tuzla_document",1)
end
function x_storage.marsh_tuzla_document_have()
  return sak.have_items_count("marsh_tuzla_document",1)~=false
end
function x_storage.give_atp_bandit_pda()
  sak_inventory.release_actor_items("bar_lucky_pda",1)
end
function x_storage.atp_bandit_pda_have()
  return sak.have_items_count("bar_lucky_pda",1)~=false
end
function x_storage.atp_bandit_pda_not_have()
  return not sak_dialog.atp_bandit_pda_have()
end
function x_storage.spawn_items_on_escape()
  sak_spawn.spawn_items_on_levels("l01_escape")
end
function x_storage.spawn_items_on_lost_village()
  sak_spawn.spawn_items_on_levels("lost_village")
end
function x_storage.spawn_items_on_marsh()
  sak_spawn.spawn_items_on_levels("marsh")
end
function x_storage.spawn_items_on_garbage()
  sak_spawn.spawn_items_on_levels("l02_garbage")
end
function x_storage.spawn_items_on_agroprom()
  sak_spawn.spawn_items_on_levels("l03_agroprom")
end
function x_storage.spawn_items_on_darkvalley()
  sak_spawn.spawn_items_on_levels("l04_darkvalley")
end
function x_storage.spawn_items_on_bar()
  sak_spawn.spawn_items_on_levels("l05_bar")
end
function x_storage.spawn_items_on_rostok()
  sak_spawn.spawn_items_on_levels("l06_rostok")
end
function x_storage.spawn_items_on_military()
  sak_spawn.spawn_items_on_levels("l07_military")
end
function x_storage.spawn_items_on_yantar()
  sak_spawn.spawn_items_on_levels("l08_yantar")
end
function x_storage.add_bar_dolg_debt_dialog()
  sak_dialog.search_npc("mil_Svoboda_regular","bar_dolg_debt_dialog","bar_dolg_debt_1")
end

function x_storage.add_foto_lisyi()
  sak.create_items(db.actor, "foto_lisyi",1)
end
function x_storage.add_mayatnik_lysyi()
  sak.create_items(db.actor, "af_mayatnik_radar_new",2)
end
function x_storage.add_udacha_art()
  sak.create_items(db.actor, "af_unknown_21_new",1)
end
function x_storage.add_sbruy_desertir()
  sak.create_items(db.actor, "dolg_gaz_outfit_m1",1)
  sak.create_items(db.actor, "wpn_abakan_m1",1)
end
function x_storage.add_video_casset()
  sak.create_items(db.actor, "video_casset",1)
end
function x_storage.batt_torch_have()
  return sak.have_items_count("batt_torch",10)~=false
end
function x_storage.give_batt_torch()
  sak_inventory.release_actor_items("batt_torch", 10)
end
function x_storage.add_doc_blood()
  sak.create_items(db.actor, "doc_blood",11)
end
function x_storage.add_bes_pda()
  sak.create_items(db.actor, "bes_pda",1)
end
function x_storage.add_tm_conserva()
  sak.create_items(db.actor, "conserva_tm",5)
end
function x_storage.add_skryaga_foto()
  sak.create_items(db.actor, "skryaga_foto",1)
end
function x_storage.add_dolg_outfit()
  sak.create_items(db.actor, "dolg_outfit",1)
end
function x_storage.add_stalkers_letter()
  sak.create_items(db.actor, "stalker_letter",1)
end

function x_storage.add_barmen_kill_stalker_1()
  sak.create_items(db.actor, "stalker_letter",1)
  sak.create_items(db.actor, "af_unknown_9_new",1)
  sak.create_items(db.actor, "af_unknown_17_new",1)
end
function x_storage.add_barmen_kill_stalker_2()
  sak.create_items(db.actor, "stalker_letter",1)
  sak.create_items(db.actor, "af_unknown_8_new",1)
end
function x_storage.add_personal_things()
  sak.create_items(db.actor, "personal_things",1)
end
function x_storage.personal_things_have()
  return sak.have_items_count("personal_things",1)~=false
end
function x_storage.give_personal_things()
  sak_inventory.release_actor_items("personal_things", 1)
end
function x_storage.add_device_pda_npc_gg()
  sak.create_items(db.actor, "device_pda_npc_gg",1)
end
function x_storage.device_pda_npc_gg_have()
  return sak.have_items_count("device_pda_npc_gg",1)~=false
end
function x_storage.give_device_pda_npc_gg()
  sak_inventory.release_actor_items("device_pda_npc_gg", 1)
end
function x_storage.flask_army_have()
  return sak.have_items_count("flask_army",1)~=false
end
function x_storage.add_scarman_remember()
  sak.send_tip("Этот мужик со шрамом... Кто-то знакомый, вроде?","Воспоминание",2,20,"g_g","rememb","yellow")
end

function x_storage.luber_ammunition()
  treasure_manager.get_treasure_manager():give_treasure("aver_secret_luber_ammunition")
end
function x_storage.rashpil_treasure()
  treasure_manager.get_treasure_manager():give_treasure("dsc_secret_rashpil")
end
function x_storage.add_rashpil_letter()
  sak.create_items(db.actor, "rashpil_letter",1)
end
function x_storage.have_rashpil_letter()
  return sak.have_items_count("rashpil_letter",1)~=false
end
function x_storage.give_rashpil_letter()
  sak_inventory.release_actor_items("rashpil_letter",1)
end
function x_storage.rashpil_2treasure()
  local obj1 = g_sim:story_object(story_ids.dsc_inventory_box_09)
  local rand = lua_random(10,20)
  for a=1,rand do
      g_sim:create("grenade_f1",obj1.position, obj1.m_level_vertex_id, obj1.m_game_vertex_id, obj1.id)
  end
  local spot=lua_random(1,4)
  level.map_add_object_spot(obj1.id, "crlc_big_treasure"..spot, "Тайник с гранатами")
end
function x_storage.lost_pda_1spot()
  local obj1 = g_sim:story_object(story_ids.aver_metka_01)
  if obj1 then
  local spot=lua_random(1,4)
  level.map_add_object_spot(obj1.id, "crlc_big_treasure"..spot, "Вроде как пещерка со складом должна быть.")
  end
end

function x_storage.add_belt_2_art()
  sak.create_items(db.actor, "belt_2_art",1)
end
function x_storage.add_belt_3_art()
  sak.create_items(db.actor, "belt_3_art",1)
end
function x_storage.add_belt_4_art()
  sak.create_items(db.actor, "belt_4_art",1)
end
function x_storage.add_belt_5_art()
  sak.create_items(db.actor, "belt_5_art",1)
end
function x_storage.add_belt_6_art()
  sak.create_items(db.actor, "belt_6_art",1)
end
function x_storage.have_belt_2_art()
  return sak.have_items_count("belt_2_art",1)
end
function x_storage.have_belt_3_art()
  return sak.have_items_count("belt_3_art",1)
end
function x_storage.have_belt_4_art()
  return sak.have_items_count("belt_4_art",1)
end
function x_storage.have_belt_5_art()
  return sak.have_items_count("belt_5_art",1)
end
function x_storage.give_belt_2_art()
  sak_inventory.release_actor_items("belt_2_art",1)
end
function x_storage.give_belt_3_art()
  sak_inventory.release_actor_items("belt_3_art",1)
end
function x_storage.give_belt_4_art()
  sak_inventory.release_actor_items("belt_4_art",1)
end
function x_storage.give_belt_5_art()
  sak_inventory.release_actor_items("belt_5_art",1)
end
function x_storage.add_akkum()
  sak.create_items(db.actor, "item_battery_02",1)
end
function x_storage.give_akkum()
  sak_inventory.release_actor_items("item_battery_02",1)
end
function x_storage.add_3_big_akkum()
  sak.create_items(db.actor, "item_battery_03",3)
end
function x_storage.have_big_akkum()
  return sak.have_items_count("item_battery_03",1)
end

function x_storage.give_af_full_akkum()
  sak_inventory.release_actor_items("af_full_akkum",1)
end
function x_storage.af_full_akkum_2have()
  return sak.have_items_count("af_full_akkum",2)
end
function x_storage.give_2af_full_akkum()
  sak_inventory.release_actor_items("af_full_akkum",2)
end
function x_storage.add_trubka()
  sak.create_items(db.actor, "trubka",1)
end
function x_storage.add_af_full_akkum()
  sak.create_items(db.actor, "af_full_akkum",1)
end
function x_storage.add_vyvert_5d()
  sak.create_items(db.actor, "af_vyvert_dyn5d",1)
end
function x_storage.add_kulinar_voron_box()
  sak.create_items(db.actor, "kulinar_voron_box",1)
  sak_dialog.start_time("start_kulinar_voron_time")
end
function x_storage.kulinar_voron_box_have()
  return sak.have_items_count("kulinar_voron_box",1)
end
function x_storage.give_kulinar_voron_box()
  sak_inventory.release_actor_items("kulinar_voron_box",1)
end
function x_storage.af_full_akkum_have()
  return sak.have_items_count("af_full_akkum",1)
end
function x_storage.have_akkum()
  return sak.have_items_count("item_battery_02",1)
end
function x_storage.add_point_1_scaner()
  local obj = g_sim:story_object(story_ids.scaner_1_space_restrictor)
  if obj then
     amk.add_spot_on_map(obj.id, "crlc_small","Место установки сканера №3")
  end
end
function x_storage.add_point_2_scaner()
  local obj = g_sim:story_object(story_ids.scaner_2_space_restrictor)
  if obj then
     amk.add_spot_on_map(obj.id, "crlc_small","Место установки сканера №4")
  end
end
function x_storage.add_point_3_scaner()
  local obj = g_sim:story_object(story_ids.scaner_3_space_restrictor)
  if obj then
     amk.add_spot_on_map(obj.id, "crlc_small","Место установки сканера №5")
  end
end
function x_storage.add_uncoder()
  sak.create_items(db.actor, "uncoder",1)
end
function x_storage.give_uncoder()
  sak_inventory.release_actor_items("uncoder",1)
end
function x_storage.have_uncoder()
  return sak.have_items_count("uncoder",1)
end
function x_storage.add_new_uncoder()
  sak.create_items(db.actor, "self_made_decoder",1)
end
function x_storage.add_scaner()
  sak.create_items(db.actor, "scanner_anomaly",1)
end
function x_storage.give_scaner()
  sak_inventory.release_actor_items("scanner_anomaly",1)
end
function x_storage.have_scaner()
  return sak.have_items_count("scanner_anomaly",1)
end
function x_storage.add_scaner_artefact()
  sak.create_items(db.actor, "scanner_artefact",1)
end
function x_storage.give_scaner_artefact()
  sak_inventory.release_actor_items("scanner_artefact",1)
end
function x_storage.have_scaner_artefact()
  return sak.have_items_count("scanner_artefact",1)
end
function x_storage.add_blockpost_case()
  sak.create_items(db.actor, "quest_case_01",1)
  local obj = g_sim:story_object(story_ids.esc_quest_case_01)
  if obj then
     g_sim:release(obj, true)
  end
end
function x_storage.add_art_capsule()
  local art_capsule=(nlc_vars.capsule_ready or "af_night_star_capsule")
  local art = string.gsub(art_capsule, "_capsule", "")
  sak.create_items(db.actor, art,1)
  nlc_vars.capsule_ready = nil
end
function x_storage.add_podsumok()
  sak.create_items(db.actor, "podsumok",1)
end
function x_storage.add_bearer_cheque()
  sak.create_items(db.actor, "bearer_cheque",1)
end
function x_storage.add_2bearer_cheque()
  sak.create_items(db.actor, "2bearer_cheque",1)
end
function x_storage.add4_bearer_cheque()
  sak.create_items(db.actor, "bearer_cheque",4)
end
function x_storage.out_podsumok()
  local obj = g_sim:story_object(story_ids.esc_podsumok_01)
  if obj then
  g_sim:release(obj, true)
  end
end
function x_storage.take_drag_cat_eyes()
  sak.create_items(db.actor, "drag_cat_eyes",1)
end
function x_storage.add_razgruzka()
  sak.create_items(db.actor, "razgruzka",1)
end
function x_storage.have_im_chain()
  return sak.have_items_count("im_chain",1)~=false
end
function x_storage.add_1_ammo_ak()
  sak.create_items(db.actor, "ammo_ak",1)
end
function x_storage.add_5_ammo_ak()
  sak.create_items(db.actor, "ammo_ak",5)
end
function x_storage.add_10_ammo_ak()
  sak.create_items(db.actor, "ammo_ak",10)
end
function x_storage.add_25_ammo_ak()
  sak.create_items(db.actor, "ammo_ak",25)
end
function x_storage.give_food_by_tm()
  sak_inventory.release_actor_items("bread",10)
  sak_inventory.release_actor_items("conserva",10)
end
function x_storage.have_food_by_tm()
  return sak.have_items_count("bread",10)~=false and sak.have_items_count("conserva",10)~=false
end
function x_storage.give_food_by_shema()
  sak_inventory.release_actor_items("bread",5)
  sak_inventory.release_actor_items("conserva",5)
  vergas_lib.search_items("energy_drink",10,2)
end
function x_storage.have_food_by_shema()
  return sak.have_items_count("bread",5)~=false and sak.have_items_count("conserva",5)~=false and vergas_lib.search_items("energy_drink",10,1)~=false
end

function x_storage.actor_yantar_teleport_have()
 local obj_t=amk_vars.sak_teleport or 0
 if obj_t==0 then return false else return true end end
function x_storage.actor_yantar_teleport_not_have()
 local obj_t=amk_vars.sak_teleport or 0
 if obj_t==0 then return true else return false end end
function x_storage.actor_doktor_teleport_have()
 local obj_t=amk_vars.sak_teleport or 0
 if obj_t==0 then return false else return true end end
function x_storage.actor_doktor_teleport_not_have()
 local obj_t=amk_vars.sak_teleport or 0
 if obj_t==0 then return true else return false end end

function x_storage.give_m16()
  sak_inventory.release_actor_items("wpn_m16a3_sk1",1)
end
function x_storage.have_m16()
  return sak.have_items_count("wpn_m16a3_sk1",1)~=false
end

function x_storage.take_treasure_kirpich(first_speaker, second_speaker)
  treasure_manager.get_treasure_manager():give_treasure("mar_secret_kirpich")
end
function x_storage.give_psy_helmet()
  if sak.have_items_count("good_psy_helmet",1)~=false then
        sak_inventory.release_actor_items("good_psy_helmet",1)
       nlc_vars.psy_helmet = "good_psy_helmet"
  else
     sak_inventory.release_actor_items("bad_psy_helmet",1)
     nlc_vars.psy_helmet = "bad_psy_helmet"
  end
end

function x_storage.add_psy_helmet()
  local psy_helmet=(nlc_vars.psy_helmet or "bad_psy_helmet")
  sak.create_items(db.actor, psy_helmet,1)
  nlc_vars.psy_helmet = nil
end
function x_storage.add_val_escort_PDA()
  sak.create_items(db.actor, "und_pda",1)
  local npc=level_object_by_sid(9542)
  if npc and npc:alive() and npc.online then
  npc:set_character_community("stalker",0,0)
  else 
  local obj=g_sim:story_object(story_ids.shersh)
    if obj then
      local pk = get_netpk(obj)
      local data = pk:get()
      data.community_index=3
      pk:set(data)
    end
  end
end
function x_storage.add_2empty_bank()
  sak.create_items(db.actor, "banka",2)
end
function x_storage.give_2full_bank(first_speaker, second_speaker)
  --out_item_section_from_actor(first_speaker, second_speaker, "banka_full",2)
  sak_inventory.relocate_actor_items(second_speaker,"banka_full",2)
end
function x_storage.have_2full_bank()
  return sak.have_items_count("banka_full",2)~=false
end
function x_storage.give_full_bank(first_speaker, second_speaker)
  sak_inventory.relocate_actor_items(second_speaker,"banka_full",1)
end
function x_storage.have_full_bank()
  return sak.have_items_count("banka_full",1)~=false
end

function x_storage.have_strelok_flash()
  return sak.have_items_count("gunslinger_flash",1)
end
function x_storage.give_strelok_flash()
  sak_inventory.release_actor_items("gunslinger_flash",1)
end
function x_storage.add_strelok_flash()
  sak.create_items(db.actor, "gunslinger_flash",1)
end
function x_storage.take_detector_sakbuzz_simple()
  sak.create_items(db.actor, "detector_sakbuzz_simple",1)
end
function x_storage.take_psy_pribor()
  sak.create_items(db.actor, "decoder2",1)
end
function x_storage.give_psy_pribor()
  sak_inventory.release_actor_items("decoder2",1)
end
function x_storage.have_3_card()
  return sak.have_items_count("rad_cart",3)~=false
end
function x_storage.havent_3_card()
  return sak.have_items_count("rad_cart",3)==false
end
function x_storage.give_3_card()
  sak_inventory.release_actor_items("rad_cart",3)
end
function x_storage.add_docs_rad_psy()
  sak.create_items(db.actor, "docs_rad_psy",1)
end
function x_storage.have_docs_rad_psy()
  return sak.have_items_count("docs_rad_psy",1)~=false
end
function x_storage.give_docs_rad_psy()
  sak_inventory.release_actor_items("docs_rad_psy",1)
end
function x_storage.add_docs_2_rad_psy()
  sak.create_items(db.actor, "docs_2_rad_psy",1)
end
function x_storage.have_docs_2_rad_psy()
  return sak.have_items_count("docs_2_rad_psy",1)~=false
end
function x_storage.give_docs_2_rad_psy()
  sak_inventory.release_actor_items("docs_2_rad_psy",1)
end
function x_storage.give_same_case()
  sak_inventory.release_actor_items("same_quest_case",1)
end
function x_storage.have_psy_pribor()
  return sak.have_items_count("decoder2",1)~=false
end
function x_storage.havent_psy_pribor()
  return sak.have_items_count("decoder2",1)==false
end
function x_storage.add_klyk_med_spot1()
  sak.add_klyk_med_spots(9301)
end
function x_storage.add_klyk_med_spot2()
  sak.add_klyk_med_spots(9302)
end
function x_storage.add_klyk_med_spot3()
  sak.add_klyk_med_spots(9303)
end
function x_storage.remove_klyk_med_spot1()
  sak.remove_klyk_med_spots(9301)
end
function x_storage.remove_klyk_med_spot2()
  sak.remove_klyk_med_spots(9302)
end
function x_storage.remove_klyk_med_spot3()
  sak.remove_klyk_med_spots(9303)
end
function x_storage.add_yan_izm_spot1()
  sak.add_yan_izm_spots(9307)
end
function x_storage.add_yan_izm_spot2()
  sak.add_yan_izm_spots(9308)
end
function x_storage.add_yan_izm_spot3()
  sak.add_yan_izm_spots(9309)
end
function x_storage.remove_yan_izm_spot1()
  sak.remove_klyk_med_spots(9307)
end
function x_storage.remove_yan_izm_spot2()
  sak.remove_klyk_med_spots(9308)
end
function x_storage.remove_yan_izm_spot3()
  sak.remove_klyk_med_spots(9309)
end
function x_storage.add_rad_izm_spot1()
  sak.add_rad_izm_spots(9310)
end
function x_storage.add_rad_izm_spot2()
  sak.add_rad_izm_spots(9311)
end
function x_storage.add_rad_izm_spot3()
  sak.add_rad_izm_spots(9312)
end
function x_storage.remove_rad_izm_spot1()
  sak.remove_rad_izm_spots(9310)
end
function x_storage.remove_rad_izm_spot2()
  sak.remove_rad_izm_spots(9311)
end
function x_storage.remove_rad_izm_spot3()
  sak.remove_rad_izm_spots(9312)
end

function x_storage.add_ecolog_weapon()
  local sid_ecolog = { story_ids.bar_ecolog_professor,
                       story_ids.bar_ecolog_crush_1,
                       story_ids.bar_ecolog_crush_2,
                       story_ids.bar_ecolog_crush_3 }
  local g_sim = alife()
  for i=1,table.getn(sid_ecolog) do
  local obj = g_sim:story_object(sid_ecolog[i])
    if obj then
      g_sim:create("wpn_ak74u", obj.position, obj.m_level_vertex_id, obj.m_game_vertex_id, obj.id)
      g_sim:create_ammo("ammo_5.45x39_ap", obj.position, obj.m_level_vertex_id, obj.m_game_vertex_id, obj.id,120)
    end
  end
end
function x_storage.monolit_instruments_spawn()
  g_sim:create("monolit_instruments_case", sak.v3f(188.23,-0.14,-184.93),335280,402)
end
function x_storage.take_toz34m(first_speaker, second_speaker)
  first_speaker:transfer_item(first_speaker:object("wpn_toz34_m1"), db.actor)
  sak.relocate_item(second_speaker, "in", "wpn_toz34_m1", 1)
end
function x_storage.take_mutant_psevdodog_tail(first_speaker, second_speaker)   
  local obj = misc.spawn_into ("mutant_psevdodog_tail", db.actor)
  if obj then
      schedule.add ("t_m_p_t","sak_dialog.take_psev_tail("..obj.id..")",200)
      sak.relocate_item(second_speaker, "in", "mutant_psevdodog_tail", 1)
  end
end
function x_storage.take_psev_tail(id)
  params.obj_condition (id, 0.11)
  local objd=g_sim:object(id)
  local pk = get_netpk(objd,1)
  local data = pk:get()
  data.condition = 0.11
  pk:set(data)
end
function x_storage.take_fort_m(first_speaker, second_speaker)
  first_speaker:transfer_item(first_speaker:object("wpn_fort_m1"), db.actor)
  sak.relocate_item(second_speaker, "in", "wpn_fort_m1", 1)
end

function x_storage.have_flamethrower()
  return sak.have_items_count("wpn_flame",1)~=false
end
function x_storage.give_flamethrower()
  sak_inventory.release_actor_items("wpn_flame",1)  
end
function x_storage.have_reload_fuel()
  return sak.have_items_count("amk_kanistra",1)~=false and sak.have_items_count("amk_ballon",1)~=false
end
function x_storage.give_reload_fuel()
  sak_inventory.release_actor_items("amk_kanistra",1)  
  sak_inventory.release_actor_items("amk_ballon",1)
end
function x_storage.have_flame_reload_items()
  return sak.have_items_count("amk_kanistra",3)~=false and sak.have_items_count("amk_ballon",3)~=false
end
function x_storage.give_flame_reload_items()
  sak_inventory.release_actor_items("amk_kanistra",3)  
  sak_inventory.release_actor_items("amk_ballon",3)
end

function x_storage.have_flame_reload_kanistra()
  return sak.have_items_count("amk_kanistra",1)~=false
end

function x_storage.have_flame_reload_ballon()
  return sak.have_items_count("amk_ballon",1)~=false
end

function x_storage.take_treasure_by_toz(first_speaker, second_speaker)
  treasure_manager.get_treasure_manager():give_treasure("val_secret_oreh")
end
function x_storage.take_treasure_oreh_by_shkura(first_speaker, second_speaker)
  treasure_manager.get_treasure_manager():give_treasure("dead_secret_oreh")
end
function x_storage.fill_dead_oreh_treasure()
  local obj1=g_sim:story_object(story_ids.dc_inventory_box_cherdak_build)
  if not obj1 then return end
  sak.create_items(obj1, { "medkit_army", "antirad", "ammo_9x19_pbp", "ammo_5.45x39_ap", "ammo_vog-25p" }, 10)
end
function x_storage.take_detector_sakbuzz_elite()
  sak.create_items(db.actor, "detector_sakbuzz_elite",1)
end
function x_storage.take_level_changer_by_toz(first_speaker, second_speaker)
  sak.add_new_garbage_116()
  sak.add_new_darkvalley_117()
end
function x_storage.give_1_dm_bandits(first_speaker, second_speaker)
  dialogs.relocate_item_section(second_speaker, "wpn_toz34_m1", "out")
end
function x_storage.have_1_dm_bandits()
  return sak.have_items_count("wpn_toz34_m1",1)~=false
end
function x_storage.take_psy_shield()
  sak.create_items(db.actor, "item_psi_helmet_02",1)
end
function x_storage.give_psy_shield()
  sak_inventory.release_actor_items("item_psi_helmet_02",1)
end
function x_storage.have_psy_shield()
  return sak.have_items_count("item_psi_helmet_02",1)
end
function x_storage.not_have_psy_shield()
  return not sak_dialog.have_psy_shield()
end

function x_storage.give_flame_reload_kanistra()
  if sak.have_items_count("amk_kanistra",3)~=false then
    sak_inventory.release_actor_items("amk_kanistra",3)
    local cnt=(nlc_vars.kanistra_screw or 0)
    nlc_vars.kanistra_screw = cnt+3
  elseif sak.have_items_count("amk_kanistra",2)~=false then
    sak_inventory.release_actor_items("amk_kanistra",2)
    local cnt=(nlc_vars.kanistra_screw or 0)
    nlc_vars.kanistra_screw = cnt+2
  elseif sak.have_items_count("amk_kanistra",1)~=false then
    sak_inventory.release_actor_items("amk_kanistra",1)
    local cnt=(nlc_vars.kanistra_screw or  0)
    nlc_vars.kanistra_screw = cnt+1
  end
end

function x_storage.give_flame_reload_ballon()
  if sak.have_items_count("amk_ballon",3)~=false then
    sak_inventory.release_actor_items("amk_ballon",3)  
    local cnt=(nlc_vars.ballon_screw or 0)
    nlc_vars.ballon_screw = cnt+3
  elseif sak.have_items_count("amk_ballon",2)~=false then
    sak_inventory.release_actor_items("amk_ballon",2)
    local cnt=(nlc_vars.ballon_screw or 0)
    nlc_vars.ballon_screw = cnt+2
  elseif sak.have_items_count("amk_ballon",1)~=false then
    sak_inventory.release_actor_items("amk_ballon",1)
    local cnt=(nlc_vars.ballon_screw or 0)
    nlc_vars.ballon_screw = cnt+1
  end
end

function x_storage.have_flame_reload_not_ballons()
  local cnt=(nlc_vars.ballon_screw or 0)
  return cnt<3
end
function x_storage.have_flame_reload_all_ballons()
  local cnt=(nlc_vars.ballon_screw or 0)
  local cnt2=(nlc_vars.kanistra_screw or 0)
  return cnt>=3 and cnt2<3
end
function x_storage.have_flame_reload_not_kanistras()
  local cnt=(nlc_vars.kanistra_screw or 0)
  return cnt<3
end
function x_storage.have_flame_reload_all_kanistras()
  local cnt=(nlc_vars.ballon_screw or 0)
  local cnt2=(nlc_vars.kanistra_screw or 0)
  return cnt<3 and cnt2>=3
end
function x_storage.have_flame_reload_full()
  local cnt=(nlc_vars.ballon_screw or 0)
  local cnt2=(nlc_vars.kanistra_screw or 0)
  return cnt>=3 and cnt2>=3
end

function x_storage.clear_flame_reload_shurup()
  nlc_vars.ballon_screw = nil
  nlc_vars.kanistra_screw = nil
end
function x_storage.give_klava_feik()
  sak_inventory.release_actor_items("klava_feik",1)
end
function x_storage.add_klava()
  sak.create_items(db.actor, "klava",1)
end
function x_storage.have_klava_feik()
  return sak.have_items_count("klava_feik",1)~=false
end
function x_storage.add_klava()
  sak.create_items(db.actor, "klava",1)
end
function x_storage.take_black_tip()
  if black_tip.have_black_tip() then return end
  sak.create_items(db.actor, sak.sect_bt, 1)
end
function x_storage.out_black_tip()
  sak_inventory.release_actor_items(sak.sect_bt, 1)
end
function x_storage.have_black_tip()
  return ( black_tip.have_black_tip() ~= false )
end 

function x_storage.no_have_black_tip()
  return ( black_tip.have_black_tip() == false )
end
function x_storage.add_yad()
  sak.create_items(db.actor, "yad2",1)
end

function x_storage.give_black_tip()
  sak_inventory.release_actor_items(sak.sect_bt, 1)
end
function x_storage.take_gift_by_weapon()
  sak.add_new_military()
  treasure_manager.get_treasure_manager():give_treasure("mil_secret_kruglov")
  sak_dialog.relocate_money(db.actor, 2500, "in")
end
function x_storage.take_gift_by_flash()
  sak.add_new_military()
  treasure_manager.get_treasure_manager():give_treasure("mil_secret_kruglov")
  sak_dialog.relocate_money(db.actor, 25000, "in")
end
function x_storage.take_gift_by_fmradio()
  sak.add_new_military()
  treasure_manager.get_treasure_manager():give_treasure("mil_secret_kruglov")
  sak_dialog.relocate_money(db.actor, 5000, "in")
end
function x_storage.take_yan_secret_kruglov()
  treasure_manager.get_treasure_manager():give_treasure("yan_secret_kruglov")
end
function x_storage.take_gift_by_letter()
  --sak.add_new_military()
  sak.create_items(db.actor, "ecolog_outfit",1)
  sak_dialog.relocate_money(db.actor, 1500, "in")
end
function x_storage.take_gift_by_krysuk()
  sak.create_items(db.actor, "bandit_master_outfit",1)
end
function x_storage.has_upgrade_info()
  if db.actor:has_info("agr_krot_secret_info") or db.actor:has_info("bar_darklab_document_gain") then return true else return false end 
end
function x_storage.havent_kruglov_weapon_done()
  if db.actor:has_info("kruglov_weapon_have_1") and db.actor:has_info("kruglov_weapon_have_2") and db.actor:has_info("kruglov_weapon_have_3") and db.actor:has_info("kruglov_weapon_have_4") then return false else return true end 
end
function x_storage.have_kruglov_weapon_done()
  if db.actor:has_info("kruglov_weapon_have_1") and db.actor:has_info("kruglov_weapon_have_2") and db.actor:has_info("kruglov_weapon_have_3") and db.actor:has_info("kruglov_weapon_have_4") then return true else return false end 
end
function x_storage.volkodav_have_start()
  if db.actor:has_info("volkodav_dt_shema_start") or db.actor:has_info("volkodav_dt_art_start") then return true else return false end 
end
function x_storage.volkodav_have_done()
  if db.actor:has_info("volkodav_dt_shema_done") or db.actor:has_info("volkodav_dt_art_done") then return true else return false end 
end
function x_storage.have_af_glassbeads()
  return sak.have_items_count("af_glassbeads",1)
end
function x_storage.give_af_glassbeads()
  sak_inventory.release_actor_items("af_glassbeads",1)
end
function x_storage.have_af_dummy_glassbeads()
  return sak.have_items_count("af_dummy_glassbeads",1)
end
function x_storage.give_af_dummy_glassbeads()
  sak_inventory.release_actor_items("af_dummy_glassbeads",1)
end
function x_storage.take_af_dummy_glassbeads()
  sak.create_items(db.actor, "af_dummy_glassbeads",1)
end
function x_storage.add_agr_shema()
  sak.create_items(db.actor, "agr_shema",1)
end
function x_storage.give_x16_documents()
  sak_inventory.release_actor_items("lab_x16_documents",1)
end
function x_storage.add_x16_flash()
  sak.create_items(db.actor, "x16_flash",1)
end
function x_storage.have_x16_flash()
  return sak.have_items_count("x16_flash",1)
end
function x_storage.give_x16_flash()
  sak_inventory.release_actor_items("x16_flash",1)
end
function x_storage.have_zub_flash()
  return sak.have_items_count("zub_flash",1)
end
function x_storage.give_zub_flash()
  sak_inventory.release_actor_items("zub_flash",1)
end
function x_storage.add_zub_flash()
  sak.create_items(db.actor, "zub_flash",1)
end
function x_storage.give_agr_shema()
  sak_inventory.release_actor_items("agr_shema",1)
end
function x_storage.add_bar_ecolog_flash()
  sak.create_items(db.actor, "bar_ecolog_flash",1)
end
function x_storage.give_bar_ecolog_flash()
  sak_inventory.release_actor_items("bar_ecolog_flash",1)
end


x_storage.sidor_blowout_dialog={
  "потрясёт нас.",
  "снова ударит.",
  "будем все, как тараканы разбегаться.",
  "мало не покажется..."
}
x_storage.docent_blowout_dialog={
  "как жахнет!",
  "мало не покажется..."
}
x_storage.saharov_blowout_dialog={
  "думаю начнётся...",
  "может начаться новый цикл."
}

x_storage.blowout_when = ""

function x_storage.do_blow_time()
  local when = ""
  if ((amk_vars.blowout or -1) > -1 and (amk_vars.blowout or -1) < 5) then return end
  local name, delay = timers.check_timer("blow_shift","amk_mod.Run_Blowout_pp()")
  local diff =(delay) / 60
  if (diff < 1) then
    when = "вот-вот наверно, минут через десять, может двадцать"
  elseif (diff < 2) then
    when = "через час-другой"
  elseif (diff >= 2 and diff <=4) then  
    when = "через пару часов"
  elseif (diff > 4 and diff <=8) then  
    when = "часов через 6-7"
  elseif (diff > 8) then
    local m_h = level:get_time_hours()
    local n_h = m_h + diff
    if (n_h >= 9 and n_h < 11) then when = "утром" end
    if (n_h >= 11 and n_h < 14) then when = "днем" end
    if (n_h >= 14 and n_h < 18) then when = "после обеда" end
    if (n_h >= 18 and n_h < 22) then when = "вечером" end
    if (n_h >= 22 and n_h < 30) then when = "ночью" end
    if (n_h >= 30 and n_h < 34) then when = "завтра утром" end
    if (n_h >= 34 and n_h < 38) then when = "завтра днем" end
    if (n_h >= 38 and n_h < 42) then when = "завтра после обеда" end
    if (n_h >= 42 and n_h < 50) then when = "завтра ночью" end
    if (n_h >= 50) then when = "через день, может больше" end
  end
  sak_dialog.blowout_when = when  
end

function x_storage.select_blowout_dialog(name)
  local lines = sak_dialog[name] 
  local m = lua_random(#lines)
  return lines[m]
end

function x_storage.sidor_blowout_dialog_end()
  local m_end = sak_dialog.select_blowout_dialog("sidor_blowout_dialog")
  sak_dialog.text_talk_dialog(m_end)
end
function x_storage.docent_blowout_dialog_end()
  local m_end = sak_dialog.select_blowout_dialog("docent_blowout_dialog")
  sak_dialog.text_talk_dialog(m_end)
end
function x_storage.saharov_blowout_dialog_end()
  local m_end = sak_dialog.select_blowout_dialog("saharov_blowout_dialog")
  sak_dialog.text_talk_dialog(m_end)
end

function sak_dialog.init_module(t) -- external upgrades
 setfenv(1, sak_dialog)
 if t and type(t) == "table" then
    for k, v in pairs(t) do
        x_storage[k] = v   
        -- wprintf(" added upgrade~C0E %-25s~C0B =~C0F %s~C07", k, type(v))
    end 
 end     
 if table_mayatniks then 
    wprintf("~CF0 good mode !~C07")
    art_in_box = table_mayatniks[amk_vars.r_task_reward or 1]
 else
    sak_dialog.art_in_box = sak_dialog.table_mayatniks[amk_vars.r_task_reward or 1]
 end     
 
 prepare_dialogs() 
end

local engine_refs = {} 


local function getter_proxy(t, key)
 if type(t) ~= "table" then
    abort (" so very strange "..type(t).." = "..DumpVar(t))
 end
 
 local v = x_storage[key]
 if v ~= nil then 
    local line = InfoFmt("~T")..sprintf(" %-35s %s \tr\n", level.name(), key)
    -- wprintf("[~T]. #USE: %s", line)
    FileWrite("$logs$\\sak_dialog.log", line, #line, "mode=append")
    return v
 end
  
 if key == "_G" then
    return engine_refs._G
 end
  
 
 return rawget(t, key) or engine_refs._G[key]      
end

x_storage.test_value = "Hello hacker!"


function sak_dialog.late_init()
 for name, v in pairs(x_storage) do
   -- wprintf(" #DBG: updating~C0E sak_dialog.%s~C0B =~C0F %s~C07", name, type(v)) 
   -- sak_dialog[name] = v  
 end
 local nst = type(_G.sak_dialog)
 if nst ~= "table" then
    abort(" so strange "..nst)    
 end
 
 local mt = getmetatable(sak_dialog) or {} 
 --[[for k, v in pairs(mt) do
   if type(k) == "string" then
      wprintf("~C0E %s~C07 =~C0F %s~C07 ", k, DumpVar(v))
   end   
 end --]]
 
 engine_refs._G = _G
  
 if (mt.__index ~= getter_proxy) then
     -- orig_getter = mt.__index
     mt.__index = getter_proxy
 end      
 setmetatable(sak_dialog, mt)

 -- wprintf(" test_value 1 = %s ", sak_dialog.test_value  or 'nil')
 -- wprintf(" test_value 2 = %s ", sak_dialog.test_value2 or 'nil')
 -- wprintf(" misc type = %s ", type(sak_dialog.misc)) 
  
end