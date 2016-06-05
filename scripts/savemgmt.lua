if savemgmt == nil then
   _G.savemgmt = {}
end

savemgmt.verbosity = 3
savemgmt.candle_id = -1
savemgmt.candle_flame = -1
savemgmt.candle_section = "zone_flame_candle_self"
savemgmt.flame_section = "zone_flame_sak_small_self"
  

local qsname = user_name().."_quicksave"
local check_points = {}
local save_cmd = nil
local best_dist = 0
local svcmd  = base64_decode('c2F2ZQ==').ansi_str
local qsvcmd = base64_decode('cXVpY2tfc2F2ZQ==').ansi_str
local qsvcmd_key = base64_decode('cXVpY2tfc2F2ZV9rZXk=').ansi_str
local restrict_radius = math.pi * 0.1

-- добавление точек, около которых работает сохранение ========================================================================================================================
local function index_cp(id)
 for i,test in ipairs(check_points) do
  if test == id then return i end   
 end -- for
 return nil
end

local function insert_cp(id)
 if (id < 10) then return end
 CheckStack() 
 local i = index_cp(id)
 if i == nil then
    table.insert(check_points, id)    
 end   
end

local function logmsg (fmt, ...)
 if savemgmt.verbosity >= 5 then
    wprintf (fmt, ...)
 end
end
local function remove_cp(id)
 local i = index_cp(id)
 if i then
   ODS('[~T]. #DBG: removed check point, id = #' .. tostring(id)) 
   table.remove(check_points, i)
 end  
end 

local function add_points(proto)
 local ids = registry.sc_objects (proto, false, 7) 
 if type(ids) ~= 'table' then return end  
 for i,id in ipairs(ids) do
  insert_cp(id)   
 end 
end

local function test_range ( a, b )
 a:sub(b)
 a:mul(a)    
 local d = math.sqrt ( a.x + a.y + a.z )  
 if d < best_dist then 
    best_dist = d 
    return true
 else
    return false
 end  
end


local function cp_detected() 
  best_dist = 500                
  for i, id in ipairs(check_points) do    
    if client_obj (id) then       
       local apos = coord_params.get_obj_pos(0)
       local cpos = coord_params.get_obj_pos(id)           
       test_range ( apos, cpos )
       logmsg ( 'cp_detected(%d): id = %d, { %s }-{ %s }, best_dist = %.3f ', i, id, misc.pos2str(apos), misc.pos2str(cpos), best_dist )        
    end -- if obj         
  end -- for       
  return ( best_dist * 0.1 < restrict_radius ) 
end

local function save_execute_addr()
 local offsets = { xr3312debug = "$289390", xr3312 = "$288AF0" } 
 -- local r = 'xr'.. GetModuleInfo('xrGame.dll').release
 local build = 'xr'.. tostring(xr_build_id)..xr_build_ext
 
 local ofs = offsets [build]
  
 -- wprintf('[~T]. #DBG: checking %s, build = <%s> ', r, build) 
 if ofs ~= nil then
    return CalcPtr("xrGame.dll", ofs, "+") -- execute_method ->> CCC_ALifeSave::Execute    
 elseif save_cmd then
    
    local vft = save_cmd ^ 0     
    return vft ^ 4
 end    
end

local function enable_sv(yep) -- предполагается скрыть глубже в DLL
 wprintf("#DBG: yep = %s", tostring(yep))
 local dest = save_execute_addr()
 if yep and type(yep) == "string" then
    yep = tostring(yep)
    yep = ( yep == "true" ) or ( yep == "1" )
 elseif yep == nil then 
    yep = true
 end
 
 
 if dest == nil then
    logmsg ("[~T]. #ERROR: code $%x", 918039)
    return 
 end -- TODO: app crash schedule
  
 local norml = "55 8B EC"   -- push ebp ; mov ebp,esp
 local retn4 = "C2 04 00" 
 local s = UnlockDMA(dest, 0) 
 if string.find (s, "#SUCCESS") then    
    local curr = ReadDMA(dest, 0, "dump", 3)    
    if savemgmt.verbosity >= 7 then  wprintf ("[~T]. #DBG: current dump at %s: %s ", dest:format(), curr) end        
    if yep and ( curr == retn4 ) then
       WriteDMA (dest, 0, norml, "dump")
       logmsg ("[~T]. #DBG: function enabled")
    end    
    if (not yep) and ( curr == norml ) then
       WriteDMA (dest, 0, retn4, "dump") 
       logmsg ("[~T]. #DBG: function disabled")
    end
 else
  wprintf("~C0C#FAIL~C07: can't unlock %s ", dest:format())       
 end -- if unlocked 
end

savemgmt.tf0 = enable_sv

local function on_saved()
 db.actor:give_game_news("Игра сохранена","ui\\ui_iconsTotal", Frect():set(498,280,83,47), 0, 3000)
end

local function exec_cmd(this, s)  
  local vftable = this ^ 0
  if vftable ~= nil then
    local Execute = vftable ^ 4  -- CCC_***::Execute         
    this:call_func(Execute, "avar="..s)
  end
end

local function save_game(filename)
 -- TODO: здесь надо сверять стек вызовов на белый список
 enable_sv(true) 
 if save_cmd and save_cmd.valid then
    logmsg ('trying save indirect')
    exec_cmd(save_cmd, filename)    
 else
    logmsg ('cannot save indirect (%s ? %s) trying via console...', FormatPtr(save_cmd), GetGlobalVar('svcb') )
    get_console():execute("save "..filename)
 end       
 enable_sv(false)
end

local function save_after_sleep()
  local y,m,d,h,mn,sec,ms = game.get_game_time():get()
  local date_time = ""
  local time_h   = ui_load_dialog.AddTimeDigit(date_time, h)
  local time_min  = ui_load_dialog.AddTimeDigit(date_time, mn)
  local time_m  = ui_load_dialog.AddTimeDigit(date_time, m)
  local time_d  = ui_load_dialog.AddTimeDigit(date_time, d)
  local save_name = (time_d.." "..time_h.."."..time_min.."-"..(game.translate_string(level.name())))
  if level.name() == "l03u_agr_underground" then
    save_name = (time_d.." "..time_h.."."..time_min.."-".."Подземелье Агропрома")
    elseif level.name() == "k01_darkscape" then
    save_name = (time_d.." "..time_h.."."..time_min.."-".."Кишка")
  end
  --save_trigger = true
  save_game(save_name)
    on_saved()
  
end

local function ignite_camp(gi)
 CheckStack() 
 local obj = misc.easy_anom (savemgmt.flame_section, gi, 0)
 if obj and obj.id then
    registry.sc_objects("zone_flame_*", true, 7)
    insert_cp(obj.id)
    logmsg ("[~T]. #SUCCESS: spawned anomaly %d ", obj.id)
 else
    logmsg ("[~T]. #ERROR: easy_anom returned "..type(obj))    
 end     
end 


local function ignite_candle(gi)
 CheckStack() 
 local obj = misc.easy_anom ( savemgmt.candle_section, gi, 0)
 registry.sc_objects("zone_flame_*", true, 7) 
 candle_flame = obj.id 
end

local function popup_anom(id)
  if not g_sim:object(id) then
     wprintf("~C0C #ERROR:~C07 cannot popup flame anomaly #%d - not exists object ", id)
     campmgr.awake_flame_anoms()
     campmgr.init_level_camps()
     return  
  end

  g_sim:set_switch_online  ( id, true  )
  g_sim:set_switch_offline ( id, false )
  if not client_obj (id) then         
     misc.process_spawn()
  end
       
  local anom = client_obj (id)
  if anom then
     anom:enable_anomaly()
  end
end  

function try_save() 
 if cp_detected() then
    save_game(qsname)
    on_saved()
 else   
    local apos = level_objects[0]:position()    
    ODS ( sprintf ('[~T]. #DBG: nearest checkpoint at %.3f ', best_dist ), 255 )    
    for i, id in ipairs(check_points) do
     local obj = client_obj (id)
     local sobj = g_sim:object(id)
     if obj then 
        local dist = obj:position():distance_to(apos) or -1 
        logmsg ('[~T].~C0C #WARN: cp~C0D %5d~C07 dist_to_actor =~C0D %.3f~C07 ', id, dist )
     elseif sobj then
        logmsg ('[~T]. #DBG: cp~C0D %5d~C07 is offine, name =~C0A %s~C07 ', id, sobj:name() )          
     else
        logmsg ('[~T].~C0C #WARN:~C07 cp %5d absent, no client/server object found ~C07', id)
     end
     
    end   
 
    -- enable_sv(false)
    -- db.actor:give_game_news("Игра не сохранена :(","ui\\ui_iconsTotal", Frect():set(498,235,83,47), 0, 3000)
 end
end

_G.try_save = try_save

function savemgmt.init_module() 
 
 return true
end

function savemgmt.enable_candle_flame()          
  CheckStack()  
  if ( not savemgmt.candle_id ) or ( savemgmt.candle_id <= 0 ) then return end
  
  local v = savemgmt.verbosity
  
  -- wprintf (' candle_id = %d', savemgmt.candle_id )   
     
  local candle = client_obj(savemgmt.candle_id)
  
  if candle and strpos( candle:name(), 'candle', 1, true ) then
           
     local gi = misc.geo_info(candle)
     local anoms = misc.find_around( gi.pos, savemgmt.candle_section, 1 ) 
     
     if #anoms == 0 then            
        gi.pos.y = gi.pos.y + 0.03
        if v > 2 then wprintf( '[~T]. #DBG: spawning new candle flame ' ) end   
        ignite_candle ( gi )  
     else
        local id = anoms [1];
        if v > 2 then wprintf( '[~T]. #DBG: enabling exists candle flame ' ) end  
        
        candle_flame = id
        popup_anom (id)          
     end -- if #anoms
     savemgmt.candle_id = -1
      
  else
   wprintf('~C0C #ERROR: not found object by candle_id ~C07')  
  end -- if   
end


function savemgmt.init_kcb()
 local hot_key = ReadIni("$mod_dir$/nlc/stor.ltx", "stor", qsvcmd_key)
 -- wprintf("[~T]. #DBG: qucksave hotkey  =~C0E %s:%s~C07", DumpVar(hot_key), type(hot_key))
 -- wprintf("[~T]. #DBG: qucksave command =~C0E %s:%s~C07", DumpVar(qsvcmd), type(qsvcmd))
 SetKbdCallback ("try_save", hot_key)
 params.execute_command('unbind', qsvcmd) -- а вот нефиг потому что
end      

function savemgmt.late_init() 
 check_points = {}
 -- it
 add_points("zone_flame_small")
 enable_sv(false)
 -- ODS('[~T]. #DBG: check_points total = '..tostring(#check_points)) 
 
 -- disabling save command forever
 local svcb = GetGlobalVar('svcb')
 if svcb ~= '' then 
    save_cmd = CalcPtr(svcb, 0, '+')
 else  
    save_cmd = params.con_commands[svcmd]
 end
    
 params.disable_command(svcmd)
  
end

function savemgmt.on_wakeup()
 CheckStack()
 save_after_sleep() -- надеюсь актор заснул рядом с подушкой или матрасом
end 

function savemgmt.on_removed_objs(id_list)
 if not campmgr.handle_camps then return end

 local sim = alife()
 local ap = db.actor:position()
 local v = savemgmt.verbosity           
 
 for i,id in ipairs(id_list) do   
   remove_cp(id)
   local t = GameObjectInfo (id)
   if ( v > 4 ) and t and t.section then
      wprintf( '[~T]. #DBG: handling defeat object~C0A %-30s~C07 (%5d) section~C0A %-20s~C07 visual~C0F %-30s ~C07', t.name, id, t.section, t.visual )     
   elseif v > 4 then     
      wprintf( '[~T]. #DBG: handling defeat anonymous object %d ', id)
   end
      
   if t and t.section and (t.section == 'rastopka') then      
      local flame_id  = campmgr.flame_map [id] or -1      
      local marker_id = campmgr.markers_map [id]      
      local marker = nil
      if not g_sim:object(flame_id) then
         wprintf("[~T].~C0C #ERROR:~C07 flame #%d not exists, cannot enable. Rastopka id = %d ", flame_id, id)
         campmgr.flame_map [id] = nil 
         return 
      end
      
      
      if v > 2 then
         wprintf( '[~T]. #DBG: Ok! Rastopka %d wasted! flame_id = %d, marker_id = %d ', id, flame_id or 0, marker_id or 0 )
      end          
       
      if ( marker_id ~= nil ) then
           marker = client_obj(marker_id) 
      end

      if flame_id and marker then    
        insert_cp ( flame_id )
        -- ODS('[~T]. #DBG: trying popup flame ')    
        popup_anom ( flame_id )                
      end -- if flame & marker
      
      campmgr.flame_map[id]   = nil
      campmgr.markers_map[id] = nil
   end -- if t
   
   -- зажигание пламени свечи
   if t and t.section and (t.section == 'zone_flame_matches') then
      
      local dist = 500        

 
      if candle_flame > 0 then
         local gi = misc.geo_info(candle_flame)
         dist = gi.pos:distance_to(ap)            
         
         if dist <= 2.3 then      
            insert_cp (candle_flame)
         end         
            
      end

      if v > 2 then      
         wprintf( '[~T]. #DBG: Ok! Matches flame %d wasted! candle_flame = %d, dist_to_actor = %.3f ', id, candle_flame, dist )
      end     


      candle_flame = -1
               
   end
 end -- for
end -- on_removed_objs

enable_sv(false)

function savemgmt.after_init()
 ODS('[~T]. #DBG: neytral load complete 0')
 collectgarbage("collect")
 collectgarbage("collect")
end

function savemgmt.set_verb(v)
 savemgmt.verbosity = tonumber(v) 
end

savemgmt.after_init()
                                                                