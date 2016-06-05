--[[
 Базовая проверка 
 

--]]


-- flush !registry.adjust_spawn_pos 7859 1
-- RunCommand('DUMP_MM_STAT')
--[[
04             Machine: Word;           //CPU назначения, для 32-bit Intel это 014С (hex)
06    NumberOfSections: Word;  //Число PE-секций
08    TimeDateStamp: DWORD;    //Дата и время (число секунд с момента 16:00 31.12.1969) создания файла
0C    PointerToSymbolTable: DWORD;   //Для OBJ-файлов поле указывает на таблицу символов
                                   //Для DLL - на таблицу с отладочной информацией, но таких может
                                   // быть несколько, и лучше руководствоваться элементом 
                                   // IMAGE_DIRECTORY_ENTRY_DEBUG каталога OptionalHeader.DataDirectory
                                   // (см. далее)
10    NumberOfSymbols: DWORD;  //Кол-во символов для OBJ; для DLL справедливо предыдущее замечание
14    SizeOfOptionalHeader: Word;    //Размер дополнительной части PE-заголовка
16    Characteristics: Word;   //Различные информационные флаги; по большому счету, не влияют
                             // на процесс загрузки
18  Magic: Word;	//Сигнатура:
			//  010B для 32-битного PE,
			//  020B для 64-битного,
			//  0107 для ROM (???)
1A    MajorLinkerVersion: Byte;
1B    MinorLinkerVersion: Byte;  //Понятно без слов
1C    SizeOfCode: DWORD;     //Суммарный виртуальный размер всех секций, содержащих код
20    SizeOfInitializedData: DWORD;   //То же для инициализированных данных
24    SizeOfUninitializedData: DWORD; // и для неинициализированных
28    AddressOfEntryPoint: DWORD; //Виртуальный адрес точки входа в PE-файл (для DLL это адрес
          // DllMain, - процедуры, обрабатывающей сообщения о загрузке/выгрузке данной DLL в
          // какой-либо процесс). 0, если точки входа нет.
30    BaseOfCode: DWORD;   //  Виртуальный адрес секции с кодом. Что содержит это поле, если секций 
                         //несколько, мне точно не известно – но это и не представляет интереса
34    BaseOfData: DWORD;   //То же для данных
38    ImageBase: DWORD;    //Предпочтительный базовый адрес загрузки. Внутри DLL все абсолютные (т.е.
           // не в виде смещения от ссылающейся инструкции, а в виде адреса) ссылки на содержащиеся
           // в ней объекты формируются в предположении, что DLL загружается в память именно с
           // этого базового адреса. Если это не так, ссылки нужно корректировать при помощи
           // информации из секции перемещений (Relocation), см. раздел о коррекции ссылок
3C   SectionAlignment: DWORD;  //Все виртуальные адреса секций кратны этому числу
40    FileAlignment: DWORD;     //Для любой секции данные, помещаемые в нее, находятся в исходном
                              // файле по смещению, кратному этому числу
44    MajorOperatingSystemVersion: Word;
46    MinorOperatingSystemVersion: Word; //Тоже, вроде бы, понятно
48    MajorImageVersion: Word;
4A    MinorImageVersion: Word;
4C    MajorSubsystemVersion: Word;
4E    MinorSubsystemVersion: Word;
50    Win32VersionValue: DWORD;   //Зарезервировано. Есть мысль, что сюда положат версию
                                // Win32-эмулятора для Win64-систем
54    SizeOfImage: DWORD;  //Размер области памяти, необходимый для размещения образа PE-файла
                         //Равен виртуальному адресу, начиная с которого могла бы располагаться
                         // секция, идущая в памяти сразу за последней существующей секцией (т.е.
                         // вирт. адрес конца последней секции, дополненной до границы секции с
                         // учетом SectionAlignment)
58    SizeOfHeaders: DWORD;   //Размер области заголовков. Областью заголовков считается все
                            // пространство исходного файла до списка секций
5C    CheckSum: DWORD;     //Возможно, род цифровой подписи или вид CRC, но обычно это поле равно 0
60    Subsystem: Word;     //Для исполнимых файлов – требуемая для работы подсистема
62    DllCharacteristics: Word;  //Свойства DLL. Значения 1,2,4 и 8 зарезервированы;
                               // 2000hex означает WDM-драйвер
64    SizeOfStackReserve: DWORD;    // *
68    SizeOfStackCommit: DWORD;     // * Эти 4 полей управляют действиями,
6C    SizeOfHeapReserve: DWORD;     // * выполняемыми при загрузке EXE-файла 
70    SizeOfHeapCommit: DWORD;      // *
74    LoaderFlags: DWORD;           //Рудимент, более не используется
74    NumberOfRvaAndSizes: DWORD;       // Количество элементов в каталоге DataDirectory
    //Далее следует массив из NumberOfRvaAndSizes элементов, ссылающиеся на важные структуры данных,
    // такие как: секция импорта, секция экспорта, секция ресурсов и т.п.
    //Лучше для доступа к этим структурам применять именно DataDirectory, а не искать нужную
    // секцию путем перебора всех секций, т.к. в одной секции могут (теоретически) находиться
    // сразу несколько управляющих структур (напр., и таблицы импорта, и экспорта)
78    DataDirectory: packed array[0..IMAGE_NUMBEROF_DIRECTORY_ENTRIES-1] of TImageDataDirectory;                                        
--]]


local code_size = 39 * 0x10000
local code_ofs  = 0x1000

local p = CalcPtr("luaicp.dll", 0, "+")
local base = p
wprintf("[~T/~U/~B]. #DBG: interceptor at %s", base:format())

local ofs = p:read(0x3C, "int")
local pe = CalcPtr(p, ofs, "+")
local ib = pe:read_ptr(0x34)

local delta = (p - ib) 
if delta then 
   delta = delta.addr
   -- wprintf(" ptr = %s, PE header = %s, IB = %s, delta = $%x ", p:format(), pe:format(), ib:format(), delta)
else
   delta = 0
end 


-- 

local scnt = pe:read(0x06, "word")
local ohsz = pe:read(0x14, "word")
local rel = pe:read(0xA0, "int")

local ddsz = pe:read(0x74, "dword")

if ddsz > 5 then
   local relofs = pe:read(0x78 + 5*8, "dword")
   -- wprintf(" Data Directory size = %d, reloc table = + 0x%x ", ddsz, relofs)
     
end

local sclist = CalcPtr(pe, 0x18 + ohsz, "+")
local schsz  = 0x28

-- wprintf(" sections = %d, opt hdr size = 0x%x, sclist at %s ", scnt, ohsz, sclist:format())
local sections = {}
local shcopy = GetGlobalVar("GreatShadow")


-- section enum outer loop  
for i = 1,scnt do
  local schdr = CalcPtr(sclist, (i - 1) * schsz, "+")
  local scnm = schdr:read(0, "ansi", 8) 
  -- 8: Misc, 0x0C: RVA, 0x10: size, 0x14: ptr-to-raw-data, 0x18: ptr-to-reloc, 0x1C: ptr-to-lns, 0x20: num-of-reloc, 0x22: num-of-lines, 0x24: chars
  local scrva = schdr:read(0x0C, "dword") 
  local scptr = CalcPtr(p, scrva, '+')
  local scsiz = schdr:read(0x10, "dword")
  
  sections[scnm] = { rva = scrva, ptr = scptr, size = scsiz }
  
  if scnm == ".text" then
     code_ofs  = scrva 
     code_size = scsiz
     if shcopy == '' then
        shcopy = GetTempBuff(16, scsiz + 0x10000)
        SetGlobalVar("GreatShadow", shcopy:format())
        -- wprintf(" allocated GreatShadow at~C0D %s~C07", GetGlobalVar("GreatShadow")) 
     else
        wprintf(" using GreatShadow at~C0D %s~C07, code_size = %d ", shcopy, code_size)
        shcopy = CalcPtr(shcopy, 0, '+') -- text convert 
     end
     local src = CalcPtr(base, scrva, '+')
     shcopy:copy_mem(src, scsiz)         -- copy code section full
          
     -- FileWrite("$logs$\\luaicp."..base:format(), shcopy, scsiz) 
  end
   
  
  -- wprintf("[~T/~B]. #DBG: section %2d =~C0F %-8s~C07, RVA =~C0D $%08x~C07, PTR = ~C0D %s~C07 ", i - 1, scnm, scrva, scptr:format())
  
  if scnm == ".reloc" then
    local ofs = 0
    local cur = scptr
    local rva = 0
    local bsz = 1
    local pcnt = 0
    local bcnt = 0
    local hmod = base.addr                                        
      
    
    -- block parsing loop
    while (ofs < scsiz) do
     
    
     rva = cur:read(0, "dword")    
     bsz = cur:read(4, "dword")
     if (rva == 0) or (bsz == 0) then break end
     bcnt = bcnt + 1
     
     if rva > 0x300000 then
        -- wprintf("~C0B[%s]~C07 rva =~C0F 0x%08X~C07, block size =~C0D %d~C07", cur:format(), rva, bsz)
     end
     
     rva = rva - code_ofs -- action relative effective .text disposition
     local cnt = (bsz - 8) / 2 -- count of words
     
     
       
     
     --[[
     for nw = 0, cnt - 1 do
      local w = cur:read (8 + nw * 2, "word")
      
      if w > 0xfff then
        w = bit_and(w, 0xfff) -- in block offset
        local mofs = rva + w  -- whole offset
        if mofs <= code_size - 4 then    
            
          local v = shcopy:read_ptr(mofs) or 0
          if mofs > 0x300C00 and mofs < 0x300C0A then
             wprintf (" [%08x] = %s before sub $%x ", mofs, v:format(), hmod)
          end           
          v = v - hmod -- revert to 0-rva relative 
          -- shcopy:write(mofs, v, "ptr")
          pcnt = pcnt + 1
          if pcnt < 5 then
             -- wprintf(" patched at $%08x ", mofs)
          end                    
        end     
        
      end        
     end -- patch loop
     --]]
     
     pcnt = pcnt + ApplyRelocations ( cur, CalcPtr(shcopy, code_ofs, '-'), code_size + 0x10000, -hmod )
     ofs = ofs + bsz
     cur = cur + bsz
     
          
    end -- while (ofs < scsiz)
    
    
    if pcnt > 0 then
       -- wprintf("[~T/~B]. #DBG:~C0D %d~C07 patches applied to shadow copy ", pcnt)
       -- FileWrite("$logs$\\luaicp.relc", shcopy, code_size)
    end
  end -- if .reloc  
    
end -- for i


-- misc.dump_table(sections)


--[[ -- 
  1. Run test.lua for make hash array constant code
  2. Insert code from console into protection.lua
  3. Build protection.lua into LuaJIT bytecode via _make_x.script
  4. Run bin2asm for convert protection.bin into protection.asm
  5. Insert contents from protection.asm into LData.pas and rebuild launch.exe
 
--]]

-- [[ -- NLC-Gold build --
local hlist = {"8A71CE9E496D54C8E1203C48A0CCCC", "423C8CC379F8CE1B888B083601DB0D", "F004E7FE202911343D74B4758237F5", "F93B07B00BB0D9359CE085746F8171", 
                "6B173C23FDC487F8E701F75D2A332F", "F52C9E7FD250C2DCE04239019C4AF9", "52796515DE5027335672EECE17A325", "D0FAB0FB6975677339FB6943E5EE9A", 
                "2A11868A8DA5C22D5886F09AE9F0E9", "EC57FE91F4945CC0680226A1D96251", "DFD2E0CFECA8AD402ACBC0FB487FED", "CDC7FA37D3DAE567DF6E4D23FC1A6A", 
                "4AA82D0A6DF9CBFD944C8FB64B7CFF", "EEA9323721E860041D967986925788", "8D11FBD6550448ACBE2C2EBBA19A0C", "FA43D189102588A6813B44F77902CC", 
                "6CC36F20E4E9B3C0285E45FDCDBBBE", "73B77F70DBE1376A1F002FD3CF3A15", "4DF1251B06F64EFDD6BBFBE6DBA2C8", "850E5E21E0F696E2EA3BD1E5479DB6", 
                "C1D3E9A1A689C314A332F66921A475", "6E2CDB556467E0E42555592324E806", "9A85E78783469A31ED4C8AD877E0D9", "C303F9CAE7D832804EF12480ADEE28", 
                "F3A4B374524B0A4AB1DBFF87202F6D", "461904FF395151CE6220CBA2D08AE2", "76DE0C3D01FADE92EC2B62E106AB07", "7C224B25E4D9A51F6A6E8CED2AF744", 
                "CE6B776C12B2CF8BE28266EEA70C8F", "147E1FD08FDE9C835F5E1F9EB6ADFC", "269BB83C57754D0B84B0831E4FCC80", "6E492E9AA9A798C17A1D4974BC334B", 
                "2442A6ECB970F59EFD22C31873EAA5", "02D4FF80713ABB79A90E18C704928E", "A3AE52FCBA1B9EA67F5190AE6229F1", "28926109DBDA803F7A058D9BFFF908", 
                "762D0D1D2C99DC43860DEC853689F7", "F4A05324AE13B8F4DBD11198804730", "4FEC167ACB76050C2D20EDE64F3859", "CC5D11B72456A1EFBB8E27314A6880", 
                "3595116F393FC54E53262DB000DF7E", "D5C233A4BFEA9D6C28F4D0143FA50F", "41963BCDC33BD8941CF7E80F502590", "CBF49DEF8A62DE1F622F82086E4CC5", 
                "DB480EC6AF867D236459D4EEBF3D9F", "793D3F19BCF4B9C0566DE3A8CA331E", "60A75DC43B052AB940367319F75AE8", "829A72EAA009E4789F96AE37A8F738", 
                "9E7CDBE01996E54D3027848A852D29" }
local last_ofs = '0x00300000:0x00316400'
--]]                

if nil == dfx then
 _G.dfx = {}
end

                
dfx.dump = ""
dfx.corr_eff = 0.01

-- ofs =    40, block at $02DC0000, hash result = DEA8E0300CB8880FB374C406D19334 <> saved D633AE5F8A68B306BBA083032CAF37
-- ofs =    48, block at $02E40000, hash result = 7D892F2000E740E511E254BB39C8D4 <> saved 82DB3AA422D3EE7A3CC11137AEA400

local traps = { { 1470, 1500 }, { 1870, 2050 }, { 3200, 3300 }, { 3700, 3900 } }
local tr_id = math.random(1, #traps)

local function check_icp(fixed)
 local seg = 0
 local limit = code_size 
 local ofs = fixed or math.random(0, #hlist - 1)
 local idx = ofs + 1
 
 local mem_ofs = ofs * 0x10000
  
 if mem_ofs + 0x10000 >= code_size then 
    return false
 end
 
 dfx.last_ofs = sprintf("0x%08x:0x%08x", mem_ofs, code_size) 
 
 local opacity = 0.358
    
 if xvars then
    opacity = xvars.__opacity or 1.0    
    xvars.__opacity = opacity
 end                      
 
 local ptr = CalcPtr(shcopy, mem_ofs, '+')
 local rh, rhs, rhr 
 local bsize = 0x10000
 
 if code_size - mem_ofs < bsize then
    bsize = code_size - mem_ofs
 end  
 

 rhs, rhr, rh = ptr:calc_hash(bsize, '$12030095E6BB1FA4')
 -- CalcPtr(rh, 0, '+').rhashr
 local cut = rhs:sub(-30)
  
 if fixed >= 0 then
   if dfx.dump ~= "" then      
      dfx.dump = dfx.dump..', '
      if fixed % 4 == 0 then
         dfx.dump = dfx.dump .. "\n                "
      end   
   end
   
   
   dfx.dump = sprintf('%s"%s\"', dfx.dump, cut)  
 end
 
 if ( hlist[idx] or '' ) ~= cut then
    wprintf(" ofs =~C0D %5d~C07, block at~C0D %s~C07, size =~C0D $%05X~C07 hash result =~C0F %s~C07 ", ofs, ptr:format(), bsize, cut)
    -- SleepEx(50)
    if xvars then
      opacity = opacity * 0.95
      xvars.__opacity = opacity
    end    
     
 end    
 
 return true
end
-- xvars.__opacity = 2.55

-- test all

local last = math.floor(code_size / 0x10000) 
for i = 0, last  do
  check_icp(i, i < last)
end

dfx.laps_count = check_icp


function stability_check()
  -- check_icp() -- one segment check

  local range = traps[tr_id]    
  local gvid = range[1]
    
  if client_obj and xvars then
     local obj = client_obj(0)
     gvid = obj:game_vertex_id()
  else
     return    
  end

  local opacity = xvars.__opacity or 1.0    
  -- TODO: reaction block, must be moved out there    
  if (opacity < 0.5) and (gvid >= range[1]) and (gvid < range[2]) then
     -- wprintf("~C0C #FATAL:~C07 buffer overrun detected!!!")
     for i = 1,1000 do
       ptr = CalcPtr("luaicp.dll", math.random(0, code_size + 0x2E8AC), "+")
       ptr:unlock(0)      
       ptr:write(0, 0, "int")
       dfx.corr_eff = dfx.corr_eff + 0.1  
     end    
  end --
end

if AddRegularTask then
   AddRegularTask("rare_checks", stability_check, nil, 0, 5000) 
end

-- wprintf(" opacity = %.3f ", xvars.__opacity or -1)

if true then
   return
end
