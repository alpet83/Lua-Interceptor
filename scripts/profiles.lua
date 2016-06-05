if _G.profiles == nil or type (_G.profiles) ~= "table" then
   wprintf(" profiles registered in namespace ")
   _G.profiles = {}   
end

if _G.sak_dialog == nil then
   LuaSafeCall("$game_scripts$\\sak_dialog.script", "#file:^dbg")
end

sak_dialog.bun_docs_have = function ()
  return sak.have_items_count("bun_document",1)
end

local x_cover = {}

function x_cover.give_bun_docs(first_speaker, second_speaker)
  sak_dialog.out_item_section_from_actor(first_speaker, second_speaker, "bun_document", "all")
end
function x_cover.x8_docs_have()
  return sak.have_items_count("x8_document",1)
end
function x_cover.give_x8_docs(first_speaker, second_speaker)
  local kol=sak_dialog.out_item_section_from_actor(first_speaker, second_speaker, "x8_document", "all")
  local n=(nlc_vars.kol_x8_docs or 0)
  nlc_vars.kol_x8_docs = (n+kol)
end
function x_cover.show_x8_docs(first_speaker, second_speaker)
  local kol=#sak_inventory.rucksack["x8_document"]
  sak.relocate_item(db.actor, "out", "x8_document", kol)
  nlc_vars.kol_x8_docs = (kol)
end
function x_cover.return_x8_docs(first_speaker, second_speaker)
  local n=(nlc_vars.kol_x8_docs or 0)
  sak.relocate_item(second_speaker, "in", "x8_document", n)
  nlc_vars.kol_x8_docs = 0
end
function x_cover.x8_docs_all_have()
  local n=(nlc_vars.kol_x8_docs or 0)
  local m=amk_vars.r_task_reward or 1
  return n>(19+m)
end
function x_cover.x8_docs_not_all_have()
  return not sak_dialog.x8_docs_all_have()
end
function x_cover.add_memory_module()
  amk.spawn_item("memory_module",sak.v3f(45.0,-22.48,51.25),3710,12129)
  amk.spawn_item("memory_module",sak.v3f(45.9,-22.48,51.25),3710,12194)
end
function x_cover.add_esc_repair()
  local obj=g_sim:story_object(story_ids.esc_soldier_repair)
  if not obj then
     misc.spawn_by_name("esc_soldier_repair")
  end
end
function x_cover.add_x10_docs_botans()
local obj=g_sim:story_object(story_ids.pesh_inventory_box_0003)
  if obj then
    sobj = g_sim:create("x10_document2",obj.position, obj.m_level_vertex_id, obj.m_game_vertex_id, obj.id)
  end
end
function x_cover.x10_document2_have()
  return sak.have_items_count("x10_document2",1)
end

function x_cover.give_x10_document2()
  sak_inventory.release_actor_items("x10_document2",1)
end
function x_cover.harddisc_wiew()
  db.actor:give_talk_message("", "ui\\ui_icon_equipment",Frect():set(1700,600,100,50),"iconed_item")
end
function x_cover.x10_harddisc_have()
  return sak.have_items_count("hard_disc",1)
end
function x_cover.give_x10_harddisc(first_speaker, second_speaker)
  local kol=sak_dialog.out_item_section_from_actor(first_speaker, second_speaker, "hard_disc", "all")
  local n=(nlc_vars.kol_harddisc or 0)
  nlc_vars.kol_harddisc = n+kol
end
function x_cover.x10_harddisc_all_have()
  local n=(nlc_vars.kol_harddisc or 0)
  return n>9
end
function x_cover.x10_harddisc_not_all_have()
  return not sak_dialog.x10_harddisc_all_have()
end

function x_cover.text_code_x10()
  local n=amk_vars.x18_iq_check or 1
  local text={
  "56326", "85695", "75962", 
  "86596", "85369", "35692", 
  "39258", "54962", "47852" 
  }
  db.actor:give_talk_message("%c[255,216,186,140]"..text[n],"ui\\ui_iconsTotal",Frect():set(0,0,0,0),"simple_answer_item")
    local need = "labx10_physic_destroyable_object_000"
  local obj = registry.find_object(need, true, server_obj)
    if obj then
    if strpos(obj:name(), need,1,true) then
      local pk = get_netpk(obj,1)
      local data = pk:get()
      data.visual_name = "physics\\small_trash\\wood_board_br_01b"
      data.physic_type=3
      pk:set(data)
    end
  end
  nlc_vars.kol_harddisc = nil
end

function x_cover.add_art_junkman()
   local n=amk_vars.r_task_reward or 1
   local m=amk_vars.r_treas_items or 2  
   m=n+m-1
   local section = (sak_dialog.table_cart[m] or '?') .."_dyn"..n.."d"
   wprintf("#DBG: add_art_junkman, m = %d, art_for_spawn =~C0A %s~C07 ", m, section)
   sak.create_items(db.actor, section, 1)
end

function x_cover.add_x18_docs()
  local rand=(amk_vars.r_task_reward or 1)*5
  local points={
    398.56289672852,-1.8038463592529,308.50228881836,34227,3940,
    393.45935058594,-0.76427090167999,329.53482055664,888469,4140,
    380.36657714844,-1.4686139822006,360.37390136719,861690,4133,
    389.91040039063,0.34254065155983,363.75640869141,881004,4133,
    -16.419914245605,-3.0078587532043,552.70043945313,21972,3937
  }
  amk.spawn_item("dar_document3",sak.v3f(points[rand-4],points[rand-3],points[rand-2]),points[rand],points[rand-1])
  points={
    38.496116638184,-10.814287185669,-6.2668147087097,7333,1136,
    14.275232315063,-10.815458297729,-16.191423416138,4869,1139,
    39.742706298828,-6.1400866508484,-15.511281013489,7418,1141,
    36.299995422363,-5.6154961585999,-35.950675964355,7228,1142,
    5.0489687919617,-10.900365829468,12.071053504944,3555,1156
  }
  amk.spawn_item("dar_document5",sak.v3f(points[rand-4],points[rand-3],points[rand-2]),points[rand],points[rand-1]) 
end

function x_cover.barman_nagrad_dar_document3()
    local rand=amk_vars.r_task_reward or 1
  if rand==1 then
  sak.create_items(db.actor,"af_vyvert_dyn5d",1)
  elseif rand==2 then
  sak.create_items(db.actor,"af_medusa_dyn5d",1)
  elseif rand==3 then
  sak.create_items(db.actor,"af_ameba_slime_dyn5d",1)
  elseif rand==4 then
  sak.create_items(db.actor,"af_blood_dyn5d",1)
  else
  sak.create_items(db.actor,"af_electra_sparkler_dyn5d",1)
  end
end
function x_cover.barman_nagrad_dar_document5()
    local rand=amk_vars.r_task_reward or 1
  if rand==2 then
  sak.create_items(db.actor,"af_vyvert_dyn5d",1)
  elseif rand==3 then
  sak.create_items(db.actor,"af_medusa_dyn5d",1)
  elseif rand==4 then
  sak.create_items(db.actor,"af_ameba_slime_dyn5d",1)
  elseif rand==5 then
  sak.create_items(db.actor,"af_blood_dyn5d",1)
  else
  sak.create_items(db.actor,"af_electra_sparkler_dyn5d",1)
  end
end
function x_cover.barman_nagrad_dar_document35()
    local rand=amk_vars.r_task_reward or 1
  if rand==2 then
  sak.create_items(db.actor,"af_ameba_slug_dyn5d",1)
  elseif rand==3 then
  sak.create_items(db.actor,"af_electra_flash_dyn5d",1)
  elseif rand==4 then
  sak.create_items(db.actor,"af_rusty_kristall_dyn5d",1)
  elseif rand==5 then
  sak.create_items(db.actor,"af_cristal_flower_dyn5d",1)
  else
  sak.create_items(db.actor,"af_mincer_meat_dyn5d",1)
  end
  sak.create_items(db.actor,"medkit",5)
  sak.create_items(db.actor,"antirad",5)
end

function x_cover.add_price_arts(art_sect)
   local have_many=(nlc_vars.price_arts or 0)
   local sys_cost = system_ini():r_float(art_sect, "cost")
   have_many=have_many+sys_cost
   nlc_vars.price_arts = have_many
end

function x_cover.sherstyk_shkura_time()
  local day=level.get_time_days()
  nlc_vars.sherstyk_shkura_time = day
end

function x_cover.is_ready_time_sherstyk_shkura()
  local day=level.get_time_days()
  local old_day=(nlc_vars.sherstyk_shkura_time or 32)
  if day>old_day then return true else return false end
end
function x_cover.not_ready_time_sherstyk_shkura()
  return not sak_dialog.is_ready_time_sherstyk_shkura()
end

function x_cover.blow_time()
  if ((amk_vars.blowout or -1) > -1 and (amk_vars.blowout or -1) < 5) then return true end
end
function x_cover.no_blow_time()
  return not sak_dialog.blow_time()
end

--------------------------------- END HIDDEN FUNCTION -------------------

local function light_quit() 
 SleepEx(1000)
 ODS('bye!', 10000);
 ExitProcess("#OK")
end

local unsigned = true 

local pk =
{  48, 129, 211,  48, 129, 164,   6,   7,  42, 134,
   72, 206,  61,   2,   1,  48, 129, 152,   2,   1,
    1,  48,  32,   6,   7,  42, 134,  72, 206,  61,
    1,   1,   2,  21,   0, 255, 255, 255, 255, 255,
  255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
  255, 127, 255, 255, 255,  48,  44,   4,  20, 255,
  255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
  255, 255, 255, 255, 255, 127, 255, 255, 252,   4,
   20,  28, 151, 190, 252,  84, 189, 122, 139, 101,
  172, 248, 159, 129, 212, 212, 173, 197, 101, 250,
   69,   4,  41,   4,  74, 150, 181, 104, 142, 245,
  115,  40,  70, 100, 105, 137, 104, 195, 139, 185,
   19, 203, 252, 130,  35, 166,  40,  85,  49, 104,
  148, 125,  89, 220, 201,  18,   4,  35,  81,  55,
  122, 197, 251,  50,   2,  21,   1,   0,   0,   0,
    0,   0,   0,   0,   0,   0,   1, 244, 200, 249,
   39, 174, 211, 202, 117,  34,  87,   2,   1,   1,
    3,  42,   0,   4, 113, 120, 193,  38,   6, 241,
  192, 196,  22, 123,  48, 246, 163,   8,  56, 183,
   19,  15, 136, 132,  75,  48,  98, 214, 144,  58,
  183, 240, 133,  95,  86,  36, 109, 245, 128, 197,
  126,  14,   3,  52 }


local dat = nil


local function dump_raw_file(file_name)
       
  local data, data_sz = FileLoad(file_name, "raw")
  wprintf(" data size = %d ", data_sz)
  local ptr = CalcPtr(data, 0, "+")
  
  if ptr ~= nil then
    local s = "{ "
    for n = 0, data_sz - 1 do
        local b = ptr:read(n, "byte")      
        s = s .. sprintf("%3d, ", b)
        if n % 10 == 9 then
           s = s .. "\n"
        end     
    end
    wprintf("%s } ", s)
  end
  
  data = nil -- allow GC collect

end

function profiles.bad_profile() 
 schedule.add ('stop_game', "profiles.stop_game()", 3000)
end

function profiles.bad_signature()
 db.actor:give_game_news( "Файл профиля не подписан!", "ui\\ui_iconsTotal", Frect():set(498,280,83,47), 0, 3000 )
 profiles.bad_profile()
end

function profiles.stop_game()
 if db.actor.level_id < 2 then
    params.execute_command("disconnect", '')
 else
    params.execute_command("quit", '')
 end    
end

function profiles.on_before_save()
  -- profiles.test_player(5)
end


local function remove_equal(alist, tname)
 local tab = alist[tname] or {}
 local prev = '?'
 local upd = {}
 if type(tab) == 'userdata' then
    tab = tab(true)
 end 
                    
 for i, v in ipairs(tab) do
  if v ~= prev then 
     table.insert(upd, v)
  end  
  prev = v
 end
 alist[tname] = upd
 wprintf(" updated list:~C0F %s~C07", table.concat(upd, ','))
end 


local function public_key()
  local key = GetTempBuff(5)
  local ksz = #pk
  local result = false
  wprintf(" key size = %d ", ksz)
  for i, v in ipairs(pk) do
   key:write(i - 1, v, "byte")    
  end  
  local pub_k = { bytes = key, size = ksz } 
  return pub_k
end

local function test_signature()
  -- TODO: here must be used singularity, not player_dat
  
  local dat = player_dat("")
  if nil == dat then
     return false
  end
  local test_data = { bytes = dat.addr, size = dat.header_sz }
  local signature = { bytes = dat.ecsn, size = dat.ecsz:read(0, "byte") }
  local ok = false  
  
  if signature.size > 32 then
     ok = verify_ecs(test_data, signature, public_key())
  end                             

  return ok
end

local function test_signature2()
  local ctx = ECDSAContext()
  local dat = player_dat()

  if nil == ctx or nil == dat then
     return false
  end
  
  local pub_k = public_key()
  ctx:init("public", pub_k.bytes, pub_k.size) 
  ctx:init("signature", dat.ecsn, dat.ecsz:read(0, "byte"))                 
  local ok = ctx:verify_signature(dat.addr, dat.header_sz)   
  dat:unload()  
  return ok  
end


local function check_system_match(dat, index, ss) --
   local accum1 = 0
   local accum2 = 0
   local count = 0
   local dofs = 0
   
   local p = dat.hwbd[index] -- first target
   if xvars.anchors == nil then
      return 0, 0
   end
   local alist = xvars.anchors[index] or {}

   if type(alist) == "userdata" then
      alist = alist(true)
   end
  
   if xvars.srgtime == nil then xvars.srgtime = {} end
   
   wprintf(' insave system registration ~C0F %s~C07', xvars.srgtime[index] or '?')
                                                                               
   for hash, rh in pairs(ss) do
      local hsize = 32 -- (#hash) / 2
      local ok = 1.0        
      local sv = 1.0
      local h = rh -- CalcPtr(rh, '@ptr')
      count = count + 1                           
      
      for hofs = 0, hsize - 1 do
          -- побайтное сравнение из профиля
          local b1 = h:read(hofs, 'byte')
          local b2 = p:read(dofs + hofs, 'byte')          
          if b1 ~= b2 then
             ok = ok * 0.5             
          end            
      end
             
      local hsaved = alist [count] or "nope"
      if type(hsaved) == "userdata" then
         hsaved = CalcPtr(hsaved, 0, '@ptr').rhash
      end 
      
      if hash ~= hsaved then
         wprintf("~C0C #WARN:~C07 system/save anchor    %d.%d: %s <> %s ", index, count, hash, hsaved)         
         sv = sv * 0.1                     
      end          
      if ok < 0.7 then
         local xhash = p + dofs
         wprintf("~C0C #WARN:~C07 system/profile anchor %d.%d: %s <> %s at $%02x:%d", index, count, hash, xhash.rhash, dofs, hsize)  
         wprintf(" verify anchor~C0D %d~C07 =~C0C %f/%f~C07", count, ok, sv)         
      end

      accum1 = accum1 + ok
      accum2 = accum2 + sv     
      
      dofs = dofs + hsize           
   end                                                 
      
   -- xvars.anchors[index] = alist

   return accum1 / count, accum2 / count
end

local function kill_gg()
   get_actor_obj().condition.health = 0
end

local function xvars_corrupt()
   xvars.ugly_flag = 1.25       
   xvars.amk_vars = "wasted"
   xvars.nlc_vars = "wasted"
   xvars.anchors = "none"
end

local player_name = "nobody"

local function load_table (obj, default)
 if obj then 
  if type(obj) == 'table' then 
     return obj 
  else
     return obj(true)
  end
  
 else
  return default
 end 
end

local function new_anchors()
  local result = {}
  while table.getn(result) < 4 do
   table.insert(result, {} ) -- all profiles
  end
  local fs = getFS()  
  local fn = fs:update_path("$game_spawn$", "all.spawn")  
  result["всё_спавн_at"] = fs:get_file_age_str(fn) 
  result["created_at"]  = PreciseTime()
  result["lib_version"] = FileVersion("$fs_root$\\bin\\luaicp.dll")
  result["sub_version"] = 121225
  result["player_name"] = player_name; 
  result["pass_num"]   = -1    
  return result 
end


local alert_msg = "Проверка защиты сработала с кодом '%s' "
local dbg_signed = false


local function store_hw_bind(dat, index, ss)
   local dofs = 0
   local pap = dat.hwbd[index] -- first target, profile anchor pointer     
   if pap == nil then
      wprintf("~C0C #FATAL: hardware bind %d unassigned!~C07", index)
   end
   
   local count = 0
  
   
   if xvars.anchors == nil then
      xvars.anchors = new_anchors()      
   end
                
   local alist = xvars.anchors[index] or {}
   if type(alist) == "userdata" then alist = alist(true) end
   
   
   for hash, rh in pairs(ss) do      
      local hsize = 32                              
      count = count + 1
      wprintf(' [%d] storing anchor%d %s at~C0D $%02x~C07', index, count, hash, dofs)
      
      local s, raw, htmp = RandomHash('blublublu', 32)
      table.insert(alist, rh)
      
      for hofs = 0, hsize - 1 do
          local b = ReadDMA(rh, hofs, 'byte')
          pap:write(dofs + hofs, b, 'byte')
      end        
      
                 
      dofs = dofs + hsize        
   end
   xvars.anchors[index] = alist
   xvars.srgtime[index] = InfoFmt('[~D ~T]')   
end

-- перенос ключей из player.dat в хранимые переменные (отладочный режим только!)
local function sync_hw_bind(dat, index)
   local dofs = 0
   local p = dat.hwbd[index] -- first source     
   if p == nil then
      wprintf("~C0C #FATAL: hardware bind %d unassigned!~C07", index)
   end
   
   if index > 3 then
      wprintf("~C0C #WARN:~C07 sync_hw_bind not acceptable for %d ", index);
      return     
   end
   
   
   local count = 0
   if xvars.anchors == nil then
      xvars.anchors = new_anchors()      
   end
                
   local alist = xvars.anchors[index] or {}
   if type(alist) == "userdata" then
      alist = alist(true)      
   end
   
   if (#alist < 1) or ( type(alist[1]) ~= 'userdata' ) then
       alist = {}
       wprintf('~C0C #WARN:~C07 hash list in saved vars not exists (new entry?) for~C0D A%d~C07 ', index);       
       for nn = 1, 4 do                           
         local s, raw, htmp = RandomHash('blablabla', 32)
         table.insert(alist, htmp)
         local ph = CastToPtr(htmp)
         if ph then
            wprintf("      added temporary hash ~C0D $%s~C07 = ~C0F %s~C07", FormatPtr(htmp), ph.rhash)
         end     
       end       
       -- return 
   end
   
   for hidx, rh in ipairs(alist) do      
      local hsize = 32                              
      count = count + 1
      
      local hsrc = p + dofs
      local hdst = CastToPtr(rh)
      if hdst == nil then
         wprintf("~C0C #FATAL:~C07 alist[%d] == nil ", hidx)
         break
      end
      
      wprintf(' [%d] syncing anchor%d P2S ~C0F %s~C07 at~C0D $%02x~C07 overrides~C0A %s~C07', index, count, hsrc.rhash, dofs, hdst.rhash)
      for hofs = 0, hsize - 1 do
          local b = hsrc:read(hofs, 'byte')
          WriteDMA(hdst, hofs, b, 'byte')          
      end        
      dofs = dofs + hsize        
   end
   xvars.anchors[index] = alist
end

local function dbg_printf(msg, ...)
 if nil == last_singularity.gold then
    wprintf(msg, ...)
 end
end




function profiles.test_player (context, ext_profile)     
  local dat = false   
  wprintf(" test_player context = %d/%s ", context or -1, ext_profile or '0')   
  local gto = game.get_game_time()
  
  local year, month, day, hour, min, sec, ms = gto:get() -- получили все значения
  -- TODO: проверить участие в подписи профиля игрока
  -- wprintf('[~T]. #DBG: %d.%2d.%4d %d:%2d' , day, month, year, hour, min )
  
  local save_name = g_sim.loaded_save_name

  local frun = ( year < 2013 ) and ( month == 5 ) and ( day == 1 ) and ( hour == 12 ) and ( min < 35 ) and ( save_name == "" )  

  if (xvars.ugly_flag ~= nil) and (xvars.ugly_flag > 1) then 
      kill_gg()
  end
  
  local sin = last_singularity
  
  if sin == nil then
     abort("bad singularity")
     return "#FAIL_2"
  end
   
  if true then
     return "#OK"
  end
  
  local t_func = test_signature
    
  local aobj = engine_object(0)  
  
  if context == nil then return "#INGORE" end -- not test vars before 
  
  if sin.lock == nil then end
  
  if sin.get_profile_key then
    local nonce = '0'  
    local pn_raw = '0' 
    local pn = '0'     
    
    if ext_profile then
       dat = player_dat(ext_profile) -- must be loaded before using hashes. Permut!!!
    else
       dat = player_dat()
    end
    
    player_name = dat.name
    
    local sigd = dat.ecsz:read(0, 'dump', 8)
    
    dbg_printf(" signature prefix dump:~C0D %s~C07", sigd)
    
    unsigned = strpos ( sigd, "2A 2A 2A 2A" )          
        
    if sin.gold == nil and t_func() then -- don't try corrupt signature
     
       dbg_signed = true
       dbg_printf(" before checks profile was signed!")
    end
        
        
    if nil == dat or nil == dat.lcnt then
       wprintf(" cannot retrieve player info")
       kill_gg()
       abort(" unrecoverable problem ")
       return "#FAIL_3"
    end

         
    if sin.gold == nil and getFS():exist("$fs_root$", "gamedata.open\\scripts\\test.script") then
       LuaSafeCall("$mod_dir$\\_make.script", "#file")
    end     
    
    local anchors = xvars.anchors
    
    if anchors then
       pn_raw = anchors.pass_num       
    end
        
    if pn_raw ~= nil then
       if type(pn_raw) == "userdata" then     
         SetPointerMT(pn_raw)
         pn = pn_raw.rhash
       else
         pn = pn_raw
       end
       -- wprintf(" pn = %s, fmt: %s, raw: %s ", DumpVar(pn_raw), pn, pn_raw.rhashr)       
    end
    
    pn = tostring(pn)
    
    if #pn > 20 then
       nonce = string.sub(pn, 1, 16) -- нонс из сейва 
       dbg_printf(" nonce = %s ", nonce)
    end
    
    
    local pk, pk_raw = sin:get_profile_key('$'..nonce)       

    if strpos(pk, "INV") then
       pk, pk_raw = sin:get_profile_key('$0')
       -- wprintf('~C0C #WARN:~C07 debug fail override %s over %s ', pk, pn)
       wprintf("  hash raw: %s ", pk_raw.rhashr)
    end
    
    local ref = pk_raw.rhash
    if pk ~= ref then
       wprintf("~C0C #ERROR:~C07 bad implementation ~C0F '%s'~C0B <>~C0E '%s'~C07 ", pk, ref)
    end

    local nnonce = string.sub(pk, 1, 16) -- real init nonce
    
    local ss = sin:sys_sign( '$'..nnonce  ) -- hardware info, uniform nonce, one argument 
    
    --[[
        Оценка аппаратной привязки, с сохранением новых компьютеров при нулевом счетчике пересечения локаций.
        Максимальное число привязок к компьютерам = 3.        
    
    --]]                           
    
    local lcnt = dat.lcnt:read(0, "word") or 0 -- levels change count
    
    local lvix = db.actor.level_id
    local lvnm = dat.lvnm:read(0, "word")
    
    if lvix ~= lvnm then
       wprintf(" detected level change to %d, levels changes = %d ", lvix, lcnt)
       lcnt = lcnt + 1
       dat.lcnt:write(0, lcnt, "word")    
    end  
    dat.lvnm:write(0, lvix, "word")
    
    if (lvix >= 12) then
        t_func = test_signature2
    end        
    
   
    local best_match = 0
    local best_sv    = 0 
    local hwbf = dat.hwbf:read(0, "dword")
    local txt = sprintf("%d", hwbf)
    
    local sys_checked = false   
    local list_lvl = {}
    table.insert (list_lvl, level.name()) -- для инциализации
       
    if frun then
       hwbf = 1            
       if #pk < 48 then
          abort(" bad hash size %d", #pk) 
       end                                       
       local ncnt = dat.ncnt:read(0, "word")   
       
       wprintf('[~T]. #DBG: first run detected, lcnt = %d, ncnt = %d', lcnt, ncnt)
       xvars.anchors = new_anchors()       
       anchors = xvars.anchors      
       anchors.pass_num = pk
       anchors.prev_level = lvix
       anchors.levels_path = list_lvl 
       
       
       if 0 == hwbf then       
          dbg_printf(" registering first system")
          store_hw_bind (dat, 1, ss)  -- reset system count                             
          dat.hwbf:write(0, 1, "int")
          sys_checked = true
       else
          for hwid = 1, txt:len() do
              sync_hw_bind(dat, hwid, ss) -- копирование хэшей из профиля
          end     
       end
          
       -- new game counter       
       dat.ncnt:write(0, ncnt + 1, "word")         
       dat:flush() 
       best_match = 0.99     
    elseif anchors == nil then
       wprintf("[~T].~C0C #ERROR:~C07 invalid save detected, anchors not exists in~C0F %s~C07 ", save_name)
       xvars.anchors = new_anchors()       
       anchors = xvars.anchors      
       anchors.pass_num = pk   
       anchors.prev_level = lvix
       anchors.levels_path = list_lvl
    end     
    
   
    remove_equal(xvars.anchors, "levels_path")
    
    -- flush #profiles.lua
    -- flush #$mod_dir$\_make_x.script    
    -- flush #$fs_root$\gamedata.open\scripts\profiles.lua 
    --  
    ------------------------------------
    if not sys_checked then    
    
       
      local check_total =  function()
         wprintf(" perform checking for~C0F '%s'~C07 on level %d by set %s",  save_name, lvix, txt)
         -- проверка оборудования, из максимум трех прописанных систем 
         for hwid = 1, txt:len() do   
           -- anchors = xvars.anchors
           local atest = {}
                    
           if anchors then 
              atest = load_table ( anchors[hwid], {} )
           end
           
           if (sin.gold == nil) and (not dbg_signed) or -- добавлен допуск на поврежденные сейвы 
              (0 == table.getn(atest)) then
              wprintf(" anchors[%d] before sync: %s ", hwid, DumpVar(atest))  
              sync_hw_bind(dat, hwid, ss) -- затирание хэшей сейва хэшами из профиля
           end                       
         
           local good, sv = check_system_match(dat, hwid, ss) -- проверка совпадений с оборудованием и записью в сейве           
           if (good > best_match) or (good > 0.7 and sv > best_sv) then
              best_match = good
              best_sv    = sv
           end
           wprintf(" integrity #%d =~C0E %f/%f~C07 ~C0F %f/%f~C07 ", hwid, good, best_match, sv, best_sv)
         end -- for
       end
       
       check_total()   
         

       -- если существующие записи не подходят.
       if (best_match < 0.7) or (best_sv < 0.75) then
           local max_lvl_chg = 1
          
           if sin.gold == nil then     
              max_lvl_chg = 20
           elseif #anchors[1] == 0 then    
             wprintf(" ~C0C #ERROR: first system config was lost!~C07")             
           end   
           
           local prev_lvl = anchors.prev_level or -1
           list_lvl = load_table (anchors.levels_path, {})
           
                        
           if prev_lvl ~= lvix then
              wprintf("[~T]. #DBG: level changed from~C0D %d~C07 to~C0D %d~C07", prev_lvl, lvix)               
              anchors.prev_level = lvix
              table.insert (list_lvl, level.name()  )              
              anchors.levels_path = list_lvl         
              wprintf("     updated list_lvl =~C0A %s~C07 ", hwbf, table.concat(list_lvl, ', '))    
           end
           
            
           if table.getn(list_lvl) <= max_lvl_chg and hwbf < 111 then 
              if hwbf == 0 then hwbf = 1 end --
              wprintf(" [~T].~C09 #DBG: registered new system, set = %d ~C07", hwbf) 
                          
              if not dbg_signed then        
                hwbf = hwbf * 10 + 1    
                txt = sprintf("%d", hwbf)      
                dat.hwbf:write(0, hwbf, "dword")          
                store_hw_bind(dat, #txt, ss)    
                dat:flush()
                -- dump_table(anchors)
                wprintf(" and one attempt more...")
                check_total()                
              end  
                                                           
           else
              list_lvl = list_lvl or {} 
              wprintf(" hwbf = %3d, list_lvl =~C0A %s~C07 ", hwbf, table.concat(list_lvl, ', ')) 
                                            
              wprintf('[~T].~C0C #FATAL(%d): hw check error code = %s ~C07', table.getn(list_lvl), FormatPtr(-42784))     
              sin.alert( sprintf(alert_msg, " не соответствие системы ") )
              if sin.gold == true then
                 xvars_corrupt()
                 schedule.add('bad_profile', 'profiles.bad_profile()', 2000) -- #FAIL
                 -- misc.release_obj(0, "infarct_1")
                 g_sim:release(g_sim:object(0),true)
                 db.actor = false                 
                 light_quit()
              else
                 dbg_printf(" levels path: %s ", DumpVar(list_lvl))   
              end        
           end
        else
         sys_checked = true                   
    
        end -- if best_match
                         
    end
    
    if strpos(pk, 'INVALID_KEY') then 
       abort(' invalid file specified, context = 24 ') -- #FAIL
    end        

    local lcount = table.getn(list_lvl)

    if not frun and (best_sv < 0.59)  then
       if sin.gold then
         sin.alert( sprintf(alert_msg, " сохранение несовместимо с профилем! "..tostring(lcount)) )
         g_sim:release(g_sim:object(0),true)
         db.actor = nil       
         light_quit()
       end   
    end

    -- wprintf('[~T]. #DBG: profile_key =~C0F %s~C07', pk)
    -- wprintf('[~T]. #DBG: saved_key   =~C0F %s~C07', pn)    
    
    -- wprintf(" pk = %s, raw: %s ", DumpVar(pk_raw), pk_raw:read(0, "dump", 32))    
    if pk ~= pk_raw.rhash then
       wprintf("~C0C #ERROR:~C07 bad permutation(?) ~C0F '%s'~C0B <>~C0E '%s'~C07 ", pk, pk_raw.rhash)
       sin.alert( sprintf(alert_msg, " internal error bad_imp ") )       
       abort(" cannot continue ")       
    end
    
    if frun then
       -- первичное сохранение ключа профиля в сейве              
    elseif pk ~= pn and lcnt >= 0 then -- чужой профиль детектед по отношению к сейву              
       if sin.gold == true and (not frun) then
         wprintf(" pk =~C0E %s~C07", pk)
         wprintf(" pn =~C0F %s~C07", pn)         
         wprintf('[~T].~C0C #FATAL: hw check error code = %s ~C07', FormatPtr(-30513))
         xvars_corrupt()       
         schedule.add('bad_profile', 'profiles.bad_profile()', 2000) -- #FAIL
         sin.alert( sprintf(alert_msg, " главный ключ не подходит для сейва "))
         misc.release_obj(0, "infarct_2")
         db.actor = nil       
         light_quit()         
       elseif sin.gold == nil then
         wprintf("[~T].~C0C #FATAL: primary key not for this save: %s <> %s, lcnt = %d", pk, pn, lcnt)
         xvars.anchors.pass_num = pk 
       end  
    else 
       wprintf('[~T]. #DBG: all ok')
    end -- first time run          
    
    if sin.gold == nil then
       misc.dump_table(xvars.anchors(true))
    end                        
    
    dat:flush() -- sync gvid, lvid, crc
    
    local ok = t_func()
    
    if (lvix > 1) or (#list_lvl > 2) then
             
       if not ok then     
          sin.alert( sprintf(alert_msg, "не подписан профиль") )          
          if sin.gold then  schedule.add('bad_profile', 'profiles.bad_signature()', 2000) end          
       end
    end          
    
    if not unsigned and not ok then
       sin.alert( sprintf(alert_msg, "подпись профиля устарела, требуется переподписать player.dat") )
    end
    
    if context and context == 23 then
       return ok -- проверка на разрешение автосейва
    end
    
    if sin.gold == nil then
       if ok then
          wprintf("~CA0 profile signature valid!~C07")
       else
          wprintf("~C0C profile signature absent/invalid!~C07")
       end   
       -- ok = true            
    end
    
    if ok then 
      SetGlobalVar("change_level_allowed", "yes")  -- разблокировка переходов
    end
    
    -- xvars.anchors.pass_num = nil
    anchors = xvars.anchors
    
    if aobj and aobj.save_stat_event and anchors.created_at then
       local ts = ( anchors.created_at - 41900 ) * 24 * 60 
       local msg = sprintf("P=%s;NGS=%.0f", user_name() or 'nil', ts)
       if frun then msg = msg.."(NOW)"; end
       aobj:save_stat_event(0x102, msg)
    end
    
  else
    sin.alert( sprintf(alert_msg, "get_profile_key == nil") )
    abort('#FATAL: get_profile_key not supported')
    return "#FAIL_1"  
  end  
  
  if sin.unlock == nil then end 
 
  xvars.ugly_flag = 0.01
  if ext_profile then 
     return dat
  else  
     return "#OK"
  end 
end


local function multi_check()
 if test_signature() and test_signature2() then
    wprintf("[~T].~C0A #AWESOME!~C07 signature valid in both checks ")
 else
    wprintf("[~T].~C0C #FAIL: Signature is not valid ")
 end
end
                              
function profiles.init_module()
 -- wprintf("%s", profiles.test_player(0))
 SetGlobalVar("change_level_allowed", "yes") -- разблокировка переходов
 sak_dialog.init_module(x_cover)  
end

function profiles.regular_test() 
end


function profiles.dev_support()
 if nil ~= LuaCompile and FileExists("$mod_dir$\\autorun.lua") then
    LuaSafeCall('$mod_dir$\\autorun.lua', '#file') 
 end
end

function profiles.late_init()
 -- profiles.test_player(1) --
 AddRegularTask("mem_lost_check", profiles.regular_test, nil, 0, 5000)
 schedule.add('dev_support', 'profiles.dev_support()', 3000)   
end

function profiles.verify_pk(ref)
 local dat = player_dat()
 -- wprintf("[~T]. #DBG: pk = %s, ref = %s ", dat.puid.rhash, ref)
 return (dat.puid.rhash == ref) 
end

function profiles.compare(ref)
 local dat = player_dat()
 local dtr = player_dat(ref)
 
 if (dat and dtr) then  
   wprintf("[~T]. #DBG: current = %s, ref = %s ", dat.puid.rhash, dtr.puid.rhash)
   return dat.puid.rhash == dtr.puid.rhash
 end 
 return false
end

function profiles.check_trusted(filename, nonce)
 local sig = filename:gsub('.%w+$', '.sig')

 local txt, txt_sz = FileLoad(filename, 'raw') 
 local ref, ref_sz = FileLoad(sig, 'raw') -- rhash + signature
   
 -- wprintf(" txt_sz = %d, ref_sz = %d ", txt_sz, ref_sz)
 
 if ref_sz ~= 74 then return end
   
 local hs, hsr, rh = txt:calc_hash(txt_sz, nonce or 0)
 -- wprintf(' signature dump:~C0D %s ~C07', ref:read(0, 'dump', ref_sz))  
 ref = ref + 42
 local ref_hash = ref.rhash;
  
 -- wprintf(' txt_hash =~C0F %s~C07 ', CastToPtr(rh).rhash)
 -- wprintf(' ref_hash =~C0E %s~C07 ', ref_hash)
 return ( ref_hash == hs )
end

init_priority = 1000 
-- test_player(10)