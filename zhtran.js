// 网页简繁体转换達到
// 日期:2007年4月1日
// 作者:niker
// 本js用于客户在网站页面选择繁体中文或简体中文显示，默认是正常显示，即简繁体同时显示
// 在用户第一次访问网页时,会自动检测客户端语言进行操作并提示.此功能可关闭
// 本程序只在UTF8编码下测试过，不保证其他编码有效

// -------------- 以下参数大部分可以更改 --------------------
//s = simplified 简体中文 t = traditional 繁体中文 n = normal 正常显示
var zh_default = 'n'; //默认语言，请不要改变
var zh_choose = 's'; //当前选择
var zh_expires = 7; //cookie过期天数
var zh_class = 'zh_click'; //链接的class名，id为class + s/t/n 之一
var zh_style_active = 'font-weight:bold; color:green;'; //当前选择的链接式样
var zh_style_inactive = 'color:blue;'; //非当前选择的链接式样
var zh_browserLang = ''; //浏览器语言
var zh_autoLang_t = true; //浏览器语言为繁体时自动进行操作
var zh_autoLang_s = false; //浏览器语言为简体时自动进行操作
var zh_autoLang_alert = false; //自动操作后是否显示提示消息
//自动操作后的提示消息
var zh_autoLang_msg = '歡迎來到本站,本站爲方便台灣香港的用戶\n1.采用UTF-8國際編碼,用任何語言發帖都不用轉碼.\n2.自動判斷繁體用戶,顯示繁體網頁\n3.在網頁最上方有語言選擇,如果浏覽有問題時可以切換\n4.本消息在cookie有效期內只顯示一次';
var zh_autoLang_checked = 0; //次检测浏览器次数,第一次写cookie为1,提示后为2,今后将不再提示


//判断浏览器语言的正则,ie为小写,ff为大写
var zh_langReg_t = /^zh-tw|zh-hk$/i;
var zh_langReg_s = /^zh-cn$/i;

//简体繁体对照字表,可以自行替换
var zh_s = '周托奸舍舍辟辟厘厘向向喂喂松松只只症暴卷范采采占制制布布志干干台台万与丑专业丛东丝丢 两准严丧个丬丰临为丽举么义乌乐乔习乡书买乱争于亏云亘亚产亩亲亵亸亿仅从仑仓仪们价众优伙会伛伞伟传伤伥伦伧伪伫体余佣佥侠侣侥侦侧侨侩侪侬俣俦俨俩俪俭 债倾偬偻偾偿傥傧储傩儿兑兖党兰关兴兹养兽冁内冈册写军农冢冯冲决况冻净凄凉减凑凛几凤凫凭凯击凼凿刍划刘则刚创删别刬刭刽刿剀剂剐剑剥剧劝办务劢动励 劲劳势勋猛勚匀匦匮区医华协单卖卢卤卧卫却卺厂厅历厉压厌厍厕厢厣厦厨厩厮县三参叆叇双发变叙迭叶号叹叽吁后吓吕吗吣吨听启吴呒呓呕呖呗员呙呛呜咏咔咙咛 咝咤咴咸呱响哑哒哓哔哕哗哙哜哝哟唛唝唠唡唢唣唤呼啧啬啭啮啰啴啸喷喽喾嗫呵嗳嘘嘤嘱噜劈嚣谑团园囱围囵国图圆圣圹场阪坏块坚坛坜坝坞坟坠垄垄垆垒垦垧垩 垫垭垯垱垲垴埘埙埚垫埯堑堕塆原墙壮声壳壶壸处备复够头夸夹夺奁奂奋奖奥妆妇妈妩妪妫姗姜娄娅娆娇娈娱娲娴婳婴婵婶媪嫒嫔嫱嬷孙学孪宁宝实宠审宪宫宽宾寝 对寻导寿将尔尘尝尧尴尸尽尽层屃屉届属屡屦屿岁岂岖岗岘岙岚岛岭岳岽岿峃峄峡峣峤峥峦崂崃崄崭嵘嵚嵛嵝脊巅巩巯币帅师帏帐帘帜带帧帮帱帻帼幂幞并么广庄庆 庐庑库应庙庞废庼廪开异弃张弥弪弯弹强归当录彟彦彻径徕御忆忏忧忾怀态怂怃怄怅怆怜总怼怿恋恳恶恸恹恺恻恼恽悦悫悬悭悯惊惧惨惩惫惬惭惮惯愍愠愤愦愿慑慭 怵懑懒懔戆戋戏戗战戬户扎扑扦执扩扪扫扬扰抚抛抟抠抡抢护报担拟拢拣拥拦拧拨择挂挚挛挜挝挞挟挠挡挢挣挤挥挦捞损捡换捣据捻掳掴掷掸掺掼揸揽揿搀搁搂搅携 摄摅摆摇摈摊撄撑撵撷撸撺擞攒敌敛数斋斓斗斩断无旧时旷旸昙昼昽显晋晒晓晔晕晖暂暧札术朴机杀杂权条来杨杩杰极构枞枢枣枥枧枨枪枫枭柜柠柽栀栅标栈栉栊栋 栌栎栏树栖样栾桊桠桡桢档桤桥桦桧桨桩梦梼梾检棂椁椟椠椤椭楼榄榇榈榉槚槛槟槠横樯樱橥橱橹橼檐檩欢欤欧歼殁殇残殒殓殚殡殴毁毂毕毙毡毵氇气氢氩氲汇汉污 汤汹沓沟没沣沤沥沦沧沨沩沪沵泞泪泶泷泸泺泻泼泽泾洁洒洼浃浅浆浇浈浉浊测浍济浏浐浑浒浓浔浕涂涌涛涝涞涟涠涡涢涣涤润涧涨涩淀渊渌渍渎渐渑渔渖渗温游湾 湿溃溅溆溇滗滚滞滟滠满滢滤滥滦滨滩滪滥潆潇潋潍潜潴澜濑濒灏灭灯灵灾灿炀炉炖炜炝点炼炽烁烂烃烛烟烦烧烨烩烫烬热焕焖焘煅糊退溜爱爷牍牦牵牺犊强犬状犷 犸犹狈狍狝狞独狭狮狯狰狱狲猃猎猕猡猪猫猬献獭玑玙玚玛玮环现玱玺珉珏珐珑珰珲琎琏琐琼瑶瑷璇璎瓒瓮瓯电画畅畲畴疖疗疟疠疡疬疮疯疱屙痈痉痒痖痨痪痫痴瘅 瘆瘗瘘瘪瘫瘾瘿癞癣癫癯皑皱皲盏盐监盖盗盘眍眦眬着睁睐睑瞒瞩矫矶矾矿砀码砖砗砚砜砺砻砾础硁硅硕硖硗硙硚确碱碍碛碜碱碹滚礼祎祢祯祷祸禀禄禅离秃秆种积 称秽秾稆税稣稳穑穷窃窍窑窜窝窥窦窭竖竞笃笋笔笕笺笼笾筑筚筛筜筝筹签简箓箦箧箨箩箪箫篑篓篮篱簖籁籴类籼粜粝粤粪粮糁糇紧絷纟纠纡红纣纤纥约级纨纩纪纫纬纭纮纯纰纱纲纳纴纵纶纷纸纹纺纻纼纽纾线绀 绁绂练组绅细织终绉绊绋绌绍绎经绐绑绒结绔绕绖绗绘给绚绛络绝绞统绠绡绢绣绤绥绦继绨绩绪绫绬续绮绯绰绱绲绳维绵绶绷绸绹绺绻综绽绾绿缀缁缂缃缄缅缆缇缈 缉缊缋缌缍缎缏线缑缒缓缔缕编缗缘缙缚缛缜缝缞缟缠缡缢缣缤缥缦缧缨缩缪缫缬缭缮缯缰缱缲缳缴缵罂网罗罚罢罴羁羟羡翘翙翚耢耧耸耻聂聋职聍联聩聪肃肠肤肷 肾肿胀胁胆胜胧胨胪胫胶脉脍脏脏脐脑脓脔脚脱脶脸腊腌腘腭腻腼腽腾膑臜舆舣舰舱舻艰艳艹艺节芈芗芜芦苁苇苈苋苌苍苎苏苘苹茎茏茑茔茕茧荆荐荙荚荛荜荞荟荠荡荡 荣荤荥荦荧荨荩荪荫荬荭荮药莅莜莱莲莳莴莶获莸莹莺莼萚萝萤营萦萧萨葱蒇蒉蒋蒌蓝蓟蓠蓣蓥蓦蔷蔹蔺蔼蕲蕴薮槁藓虏虑虚虫虬虮虽虾虿蚀蚁蚂蚕蚝蚬蛊蛎蛏蛮蛰 蛱蛲蛳蛴蜕蜗蜡蝇蝈蝉蝎蝼蝾螀螨蟏衅衔补衬衮袄袅袆袜袭袯装裆裈裢裣裤裥褛褴襁襕见观觃规觅视觇览觉觊觋觌觍觎觏觐觑觞触觯詟誉誊讠计订讣认讥讦讧讨让讪 讫训议讯记讱讲讳讴讵讶讷许讹论讻讼讽设访诀证诂诃评诅识诇诈诉诊诋诌词诎诏诐译诒诓诔试诖诗诘诙诚诛诜话诞诟诠诡询诣诤该详诧诨诩诪诫诬语诮误诰诱诲诳 说诵诶请诸诹诺读诼诽课诿谀谁谂调谄谅谆谇谈谊谋谌谍谎谏谐谑谒谓谔谕谖谗咨谙谚谛谜谝谞谟谠谡谢谣谤谥谦谧谨谩谪谫谬谭谮谯谰谱谲谳谴谵谶谷豮贝贞负贠 贡财责贤败账货质贩贪贫贬购贮贯贰贱贲贳贴贵贶贷贸费贺贻贼贽贾贿赀赁赂赃资赅赆赇赈赉赊赋赌赍赎赏赐赑赒赓赔赕赖赗赘赙赚赛赜赝赞赞赟赠赡赢赣赪赵赶趋趱 趸跃跄跖跞践跶跷跸跹跻踊踌踪踬踯蹑蹒蹰蹿躏躜躯车轧轨轩轪轫转轭轮软轰轱轲轳轴轵轶轷轸轹轺轻轼载轾轿辀辁辂较辄辅辆辇辈辉辊辋辌辍辎辏辐辑辒输辔辕辖 辗辘辙辚辞辩辫边辽达迁过迈运还这进远违连迟迩径迹适选逊递逦逻遗遥邓邝邬邮邹邺邻郁郄郏郐郑郓郦郧郸酝酦酱酽酾酿释里里巨巨鉴鉴銮錾钆钇针钉钊钋钌钍钎钏钐 钑钒钓钔钕钖钗钘钙钚钛钝钞钟钠钡钢钣钤钥钦钧钨钩钪钫钬钭钮钯钰钱钲钳钴钵钶钷钸钹钺钻钼钽钾钿铀铁铂铃铄铅铆铈铉铊铋铌铍铎铏铐铑铒铓铔铕铖铗铘铙铚 铛铜铝铞铟铠铡铢铣铤铥铦铧铨铩铪铫铬铭铮铯铰铱铲铲铳铴铵银铷铸铹铺铻铼铽链铿销锁锂锃锄锅锆锇锈锉锊锋锌锍锎锏锐锑锒锓锔锕锖锗锘错锚锛锜锝锞锟锠锡锢 锣锤锥锦锧锨锩锪锫锬锭键锯锰锱锲锳锴锵锶锷锸锹钟锻锼锽锾锿镀镁镂镃镄镅镆镇镈镉镊镋镌镍镎镏镐镑镒镓镔镕镖镗镘镙镚镛镜镝镞旋镠镡镢镣镤镥镦镧镨镩镪 镫镬镭镮镯镰镱镲镳镴镵镶长门闩闪闫闬闭问闯闰闱闲闳间闵闶闷闸闹闺闻闼闽闾闿阀阁阂阃阄阅阆阇阈阉阊阋阌阍阎阏阐阑阒阓阔阕阖阗阘阙阚阛队阳阴阵阶际陆 陇陈陉陕陧陨险随隐隶隽难雏雠雳雾霁霉霭靓静靥鞑鞒鞯鞴韦韧韨韩韪韫韬韵页顶顷顸项顺须顼顽顾顿颀颁颂颃预颅领颇颈颉颊颋颌颍颎颏颐频颒颓颔颕颖颗题颙颚 颛颜额颞颟颠颡颢颣颤颥颦颧风扬飐飑飒飓飔飕飖飗飘飙飙飞飨餍饤饥饦饧饨饩饪饫饬饭饮饯饰饱饲饳饴饵饶饷饸饹饺饻饼饽饾饿余馁馂馃馄馅馆馇馈馉馊馋馌馍馎 馏馐馑馒馓馔馕马驭驮驯驰驱驲驳驴驵驶驷驸驹驺驻驼驽驾驿骀骁骂骃骄骅骆骇骈骉骊骋验骍骎骏骐骑骒骓骔骕骖骗骘骙骚骛骜骝骞骟骠骡骢骣骤骥骦骧髅髋髌鬓魇 魉鱼鱽鱾鱿鲀鲁鲂鲄鲅鲆鲶鲈鲉鲊鲋鲌鲍鲎鲏鲐鲑鲒鲓鲔鲕鲖鲗鲘鲙鲚鲛鲜鲝鲞鲟鲠鲡鲢鲣鲤鲥鲦鲧鲨鲩鲪鲫鲬鲭鲮鲯鲰鲱鲲鲳鲴鲵鲶鲷鲸鲹鲺鲻鲼鲽鲾鲿鳀鳁鳂鳃 鳄鳅鳆鳇鳈鳉鳊鳋鳌鳍鳎鳏鳐鳑鳒鳓鳔鳕鳖鳗鳘鳙鳛鳜鳝鳞鳟鳠鳡鳢鳣鸟鸠鸡鸢鸣鸤鸥鸦鸧鸨鸩鸪鸫鸬鸭鸮鸯鸰鸱鸲鸳鸴鸵鸶鸷鸸鸹鸺鸻鸼鸽鸾鸿鹀鹁鹂鹃鹄鹅鹆鹇 鹈鹉鹊鹋鹌鹍鹎鹏鹐鹑鹒鹓鹔鹕鹖鹗鹘鹙鹚鹛鹜鹝鹞鹟鹠鹡鹢鹣鹤鹥鹦鹧鹨鹩鹪鹫鹬鹭鹯鹰鹱鹲鹳鹴鹾麦麸黄黉黡黩黪黾鼋鼌鼍鼗鼹齄齐齑齿龀龁龂龃龄龅龆龇龈龉 龊龋龌龙龚龛龟系系系弥征';
var zh_t = '週託姦舍捨辟闢厘釐向嚮喂餵松鬆只隻癥暴捲範采採佔制製布佈志幹乾臺颱萬與醜專業叢東絲丟 兩準嚴喪個爿豐臨為麗舉麼義烏樂喬習鄉書買亂爭於虧雲亙亞產畝親褻嚲億僅從侖倉儀們價眾優夥會傴傘偉傳傷倀倫傖偽佇體餘傭僉俠侶僥偵側僑儈儕儂俁儔儼倆儷儉 債傾傯僂僨償儻儐儲儺兒兌兗黨蘭關興茲養獸囅內岡冊寫軍農塚馮衝決況凍淨淒涼減湊凜幾鳳鳧憑凱擊氹鑿芻劃劉則剛創刪別剗剄劊劌剴劑剮劍剝劇勸辦務勱動勵 勁勞勢勳猛勩勻匭匱區醫華協單賣盧鹵臥衛卻巹廠廳歷厲壓厭厙廁廂厴廈廚廄廝縣三參靉靆雙發變敘疊葉號歎嘰籲後嚇呂嗎唚噸聽啟吳嘸囈嘔嚦唄員咼嗆嗚詠哢嚨嚀 噝吒噅鹹呱響啞噠嘵嗶噦嘩噲嚌噥喲嘜嗊嘮啢嗩唕喚呼嘖嗇囀齧囉嘽嘯噴嘍嚳囁呵噯噓嚶囑嚕劈囂謔團園囪圍圇國圖圓聖壙場阪壞塊堅壇壢壩塢墳墜壟壟壚壘墾坰堊 墊埡墶壋塏堖塒塤堝墊垵塹墮壪原牆壯聲殼壺壼處備複夠頭誇夾奪奩奐奮獎奧妝婦媽嫵嫗媯姍薑婁婭嬈嬌孌娛媧嫻嫿嬰嬋嬸媼嬡嬪嬙嬤孫學孿寧寶實寵審憲宮寬賓寢 對尋導壽將爾塵嘗堯尷屍盡儘層屭屜屆屬屢屨嶼歲豈嶇崗峴嶴嵐島嶺嶽崠巋嶨嶧峽嶢嶠崢巒嶗崍嶮嶄嶸嶔崳嶁脊巔鞏巰幣帥師幃帳簾幟帶幀幫幬幘幗冪襆並么廣莊慶 廬廡庫應廟龐廢廎廩開異棄張彌弳彎彈強歸當錄彠彥徹徑徠禦憶懺憂愾懷態慫憮慪悵愴憐總懟懌戀懇惡慟懨愷惻惱惲悅愨懸慳憫驚懼慘懲憊愜慚憚慣湣慍憤憒願懾憖 怵懣懶懍戇戔戲戧戰戩戶紮撲扡執擴捫掃揚擾撫拋摶摳掄搶護報擔擬攏揀擁攔擰撥擇掛摯攣掗撾撻挾撓擋撟掙擠揮撏撈損撿換搗據撚擄摑擲撣摻摜摣攬撳攙擱摟攪攜 攝攄擺搖擯攤攖撐攆擷擼攛擻攢敵斂數齋斕鬥斬斷無舊時曠暘曇晝曨顯晉曬曉曄暈暉暫曖劄術樸機殺雜權條來楊榪傑極構樅樞棗櫪梘棖槍楓梟櫃檸檉梔柵標棧櫛櫳棟 櫨櫟欄樹棲樣欒棬椏橈楨檔榿橋樺檜槳樁夢檮棶檢欞槨櫝槧欏橢樓欖櫬櫚櫸檟檻檳櫧橫檣櫻櫫櫥櫓櫞簷檁歡歟歐殲歿殤殘殞殮殫殯毆毀轂畢斃氈毿氌氣氫氬氳匯漢污 湯洶遝溝沒灃漚瀝淪滄渢溈滬濔濘淚澩瀧瀘濼瀉潑澤涇潔灑窪浹淺漿澆湞溮濁測澮濟瀏滻渾滸濃潯濜塗湧濤澇淶漣潿渦溳渙滌潤澗漲澀澱淵淥漬瀆漸澠漁瀋滲溫遊灣 濕潰濺漵漊潷滾滯灩灄滿瀅濾濫灤濱灘澦濫瀠瀟瀲濰潛瀦瀾瀨瀕灝滅燈靈災燦煬爐燉煒熗點煉熾爍爛烴燭煙煩燒燁燴燙燼熱煥燜燾煆糊退溜愛爺牘犛牽犧犢強犬狀獷 獁猶狽麅獮獰獨狹獅獪猙獄猻獫獵獼玀豬貓蝟獻獺璣璵瑒瑪瑋環現瑲璽瑉玨琺瓏璫琿璡璉瑣瓊瑤璦璿瓔瓚甕甌電畫暢佘疇癤療瘧癘瘍鬁瘡瘋皰屙癰痙癢瘂癆瘓癇癡癉 瘮瘞瘺癟癱癮癭癩癬癲臒皚皺皸盞鹽監蓋盜盤瞘眥矓著睜睞瞼瞞矚矯磯礬礦碭碼磚硨硯碸礪礱礫礎硜矽碩硤磽磑礄確鹼礙磧磣堿镟滾禮禕禰禎禱禍稟祿禪離禿稈種積 稱穢穠穭稅穌穩穡窮竊竅窯竄窩窺竇窶豎競篤筍筆筧箋籠籩築篳篩簹箏籌簽簡籙簀篋籜籮簞簫簣簍籃籬籪籟糴類秈糶糲粵糞糧糝餱緊縶糸糾紆紅紂纖紇約級紈纊紀紉緯紜紘純紕紗綱納紝縱綸紛紙紋紡紵紖紐紓線紺 絏紱練組紳細織終縐絆紼絀紹繹經紿綁絨結絝繞絰絎繪給絢絳絡絕絞統綆綃絹繡綌綏絛繼綈績緒綾緓續綺緋綽緔緄繩維綿綬繃綢綯綹綣綜綻綰綠綴緇緙緗緘緬纜緹緲 緝縕繢緦綞緞緶線緱縋緩締縷編緡緣縉縛縟縝縫縗縞纏縭縊縑繽縹縵縲纓縮繆繅纈繚繕繒韁繾繰繯繳纘罌網羅罰罷羆羈羥羨翹翽翬耮耬聳恥聶聾職聹聯聵聰肅腸膚膁 腎腫脹脅膽勝朧腖臚脛膠脈膾髒臟臍腦膿臠腳脫腡臉臘醃膕齶膩靦膃騰臏臢輿艤艦艙艫艱豔艸藝節羋薌蕪蘆蓯葦藶莧萇蒼苧蘇檾蘋莖蘢蔦塋煢繭荊薦薘莢蕘蓽蕎薈薺蕩盪 榮葷滎犖熒蕁藎蓀蔭蕒葒葤藥蒞蓧萊蓮蒔萵薟獲蕕瑩鶯蓴蘀蘿螢營縈蕭薩蔥蕆蕢蔣蔞藍薊蘺蕷鎣驀薔蘞藺藹蘄蘊藪槁蘚虜慮虛蟲虯蟣雖蝦蠆蝕蟻螞蠶蠔蜆蠱蠣蟶蠻蟄 蛺蟯螄蠐蛻蝸蠟蠅蟈蟬蠍螻蠑螿蟎蠨釁銜補襯袞襖嫋褘襪襲襏裝襠褌褳襝褲襇褸襤繈襴見觀覎規覓視覘覽覺覬覡覿覥覦覯覲覷觴觸觶讋譽謄訁計訂訃認譏訐訌討讓訕 訖訓議訊記訒講諱謳詎訝訥許訛論訩訟諷設訪訣證詁訶評詛識詗詐訴診詆謅詞詘詔詖譯詒誆誄試詿詩詰詼誠誅詵話誕詬詮詭詢詣諍該詳詫諢詡譸誡誣語誚誤誥誘誨誑 說誦誒請諸諏諾讀諑誹課諉諛誰諗調諂諒諄誶談誼謀諶諜謊諫諧謔謁謂諤諭諼讒諮諳諺諦謎諞諝謨讜謖謝謠謗諡謙謐謹謾謫譾謬譚譖譙讕譜譎讞譴譫讖穀豶貝貞負貟 貢財責賢敗賬貨質販貪貧貶購貯貫貳賤賁貰貼貴貺貸貿費賀貽賊贄賈賄貲賃賂贓資賅贐賕賑賚賒賦賭齎贖賞賜贔賙賡賠賧賴賵贅賻賺賽賾贗贊讚贇贈贍贏贛赬趙趕趨趲 躉躍蹌蹠躒踐躂蹺蹕躚躋踴躊蹤躓躑躡蹣躕躥躪躦軀車軋軌軒軑軔轉軛輪軟轟軲軻轤軸軹軼軤軫轢軺輕軾載輊轎輈輇輅較輒輔輛輦輩輝輥輞輬輟輜輳輻輯轀輸轡轅轄 輾轆轍轔辭辯辮邊遼達遷過邁運還這進遠違連遲邇逕跡適選遜遞邐邏遺遙鄧鄺鄔郵鄒鄴鄰鬱郤郟鄶鄭鄆酈鄖鄲醞醱醬釅釃釀釋裏裡巨鉅鑒鑑鑾鏨釓釔針釘釗釙釕釷釺釧釤 鈒釩釣鍆釹鍚釵鈃鈣鈈鈦鈍鈔鍾鈉鋇鋼鈑鈐鑰欽鈞鎢鉤鈧鈁鈥鈄鈕鈀鈺錢鉦鉗鈷缽鈳鉕鈽鈸鉞鑽鉬鉭鉀鈿鈾鐵鉑鈴鑠鉛鉚鈰鉉鉈鉍鈮鈹鐸鉶銬銠鉺鋩錏銪鋮鋏鋣鐃銍 鐺銅鋁銱銦鎧鍘銖銑鋌銩銛鏵銓鎩鉿銚鉻銘錚銫鉸銥鏟剷銃鐋銨銀銣鑄鐒鋪鋙錸鋱鏈鏗銷鎖鋰鋥鋤鍋鋯鋨鏽銼鋝鋒鋅鋶鐦鐧銳銻鋃鋟鋦錒錆鍺鍩錯錨錛錡鍀錁錕錩錫錮 鑼錘錐錦鑕鍁錈鍃錇錟錠鍵鋸錳錙鍥鍈鍇鏘鍶鍔鍤鍬鐘鍛鎪鍠鍰鎄鍍鎂鏤鎡鐨鎇鏌鎮鎛鎘鑷钂鐫鎳鎿鎦鎬鎊鎰鎵鑌鎔鏢鏜鏝鏍鏰鏞鏡鏑鏃旋鏐鐔钁鐐鏷鑥鐓鑭鐠鑹鏹 鐙鑊鐳鐶鐲鐮鐿鑔鑣鑞鑱鑲長門閂閃閆閈閉問闖閏闈閑閎間閔閌悶閘鬧閨聞闥閩閭闓閥閣閡閫鬮閱閬闍閾閹閶鬩閿閽閻閼闡闌闃闠闊闋闔闐闒闕闞闤隊陽陰陣階際陸 隴陳陘陝隉隕險隨隱隸雋難雛讎靂霧霽黴靄靚靜靨韃鞽韉韝韋韌韍韓韙韞韜韻頁頂頃頇項順須頊頑顧頓頎頒頌頏預顱領頗頸頡頰頲頜潁熲頦頤頻頮頹頷頴穎顆題顒顎 顓顏額顳顢顛顙顥纇顫顬顰顴風颺颭颮颯颶颸颼颻飀飄飆飆飛饗饜飣饑飥餳飩餼飪飫飭飯飲餞飾飽飼飿飴餌饒餉餄餎餃餏餅餑餖餓餘餒餕餜餛餡館餷饋餶餿饞饁饃餺 餾饈饉饅饊饌饢馬馭馱馴馳驅馹駁驢駔駛駟駙駒騶駐駝駑駕驛駘驍罵駰驕驊駱駭駢驫驪騁驗騂駸駿騏騎騍騅騌驌驂騙騭騤騷騖驁騮騫騸驃騾驄驏驟驥驦驤髏髖髕鬢魘 魎魚魛魢魷魨魯魴魺鮁鮃鯰鱸鮋鮓鮒鮊鮑鱟鮍鮐鮭鮚鮳鮪鮞鮦鰂鮜鱠鱭鮫鮮鮺鯗鱘鯁鱺鰱鰹鯉鰣鰷鯀鯊鯇鮶鯽鯒鯖鯪鯕鯫鯡鯤鯧鯝鯢鯰鯛鯨鯵鯴鯔鱝鰈鰏鱨鯷鰮鰃鰓 鱷鰍鰒鰉鰁鱂鯿鰠鼇鰭鰨鰥鰩鰟鰜鰳鰾鱈鱉鰻鰵鱅鰼鱖鱔鱗鱒鱯鱤鱧鱣鳥鳩雞鳶鳴鳲鷗鴉鶬鴇鴆鴣鶇鸕鴨鴞鴦鴒鴟鴝鴛鴬鴕鷥鷙鴯鴰鵂鴴鵃鴿鸞鴻鵐鵓鸝鵑鵠鵝鵒鷳 鵜鵡鵲鶓鵪鶤鵯鵬鵮鶉鶊鵷鷫鶘鶡鶚鶻鶖鶿鶥鶩鷊鷂鶲鶹鶺鷁鶼鶴鷖鸚鷓鷚鷯鷦鷲鷸鷺鸇鷹鸌鸏鸛鸘鹺麥麩黃黌黶黷黲黽黿鼂鼉鞀鼴齇齊齏齒齔齕齗齟齡齙齠齜齦齬 齪齲齷龍龔龕龜系係繫瀰徵';
String.prototype.tran=function(){
	var s1,s2;
	if(zh_choose=='t'){
	   s1 = zh_s;
	   s2 = zh_t;
	}else if(zh_choose=='s'){
	   s1 = zh_t;
	   s2 = zh_s;
	}else{
	   return this;
	}
	var a = '';
	var l = this.length;
	for(var i=0;i<this.length;i++){
		var c = this.charAt(i);
		var p = s1.indexOf(c);
		var be = this.charAt(i-1);
		var af = this.charAt(i+1);
		switch (c){
			case "铲":
			if ( af == "除" ){
				a += "剷";
				break;
			}
            else {
            	a += p < 0 ? c : s2.charAt(p);
              	break;
            }
			case "荡":
			if ( be == "动" ){
				a += "盪";
				break;
			}
            else {
            	a += p < 0 ? c : s2.charAt(p);
              	break;
            }
			case "台":
			if ( af == "风" ){
				a += "颱";
				break;
			}
            else {
            	a += p < 0 ? c : s2.charAt(p);
              	break;
            }
			case "甚":
			if ( af == "麼" && zh_choose=='s'){
				a += "什";
				break;
			}
            else {
            	a += p < 0 ? c : s2.charAt(p);
              	break;
            }
			case "什":
			if ( af == "么" ){
				a += "甚";
				break;
			}
            else {
            	a += p < 0 ? c : s2.charAt(p);
              	break;
            }
			case "只":
			if ( af == "身" || be == "一"){
				a += "隻";
				break;
			}
            else {
            	a += p < 0 ? c : s2.charAt(p);
              	break;
            }
			case "挺":
            if (af=="而" && this.charAt(i+2)=="走"){
              a += "鋌";
              break;
            }
            else {
            	a += p < 0 ? c : s2.charAt(p);
              	break;
            }
			case "鋌":
            if (af=="而" && this.charAt(i+2)=="走"){
              a += "挺";
              break;
            }
            else {
            	a += p < 0 ? c : s2.charAt(p);
              	break;
            }
			case "余":
            if (af=="也" && this.charAt(i+2)=="鲁"){
              a += c;
              break;
            }
            else {
            	a += p < 0 ? c : s2.charAt(p);
              	break;
            }
			case "御":
			if ( af == "园"){
				a += c;
				break;
			}
            else {
            	a += p < 0 ? c : s2.charAt(p);
              	break;
            }
			case "松":
			if ( af == "口" || be == "宽" || af == "绑" || be == "放" || be == "轻"){
				a += "鬆";
				break;
			}
            else {
            	a += p < 0 ? c : s2.charAt(p);
              	break;
            }
			case "了":
			if ( af == "解" || be == "明" || af == "望" || (af == "如" && this.charAt(i+2)=="指")){
				a += "瞭";
				break;
			}
            else {
            	a += p < 0 ? c : s2.charAt(p);
              	break;
            }
			case "瞭":
			if ( af == "解" || be == "明" || af == "如" ){
				a += "了";
				break;
			}
            else {
            	a += p < 0 ? c : s2.charAt(p);
              	break;
            }
			case "伙":
			if ( af == "夫" || be == "家"){
				a += c;
				break;
			}
            else {
            	a += p < 0 ? c : s2.charAt(p);
              	break;
            }
			case "喂":
			if ( af == "食"){
				a += "餵";
				break;
			}
            else {
            	a += p < 0 ? c : s2.charAt(p);
              	break;
            }
			case "向":
			if ( af == "往"){
				a += "嚮";
				break;
			}
            else {
            	a += p < 0 ? c : s2.charAt(p);
              	break;
            }
			case "厘":
			if ( af == "定"){
				a += "釐";
				break;
			}
            else {
            	a += p < 0 ? c : s2.charAt(p);
              	break;
            }
		    case "辟":
			if ( be == "开" || be == "精"){
				a += "闢";
				break;
			}
            else {
            	a += p < 0 ? c : s2.charAt(p);
              	break;
            }
			case "舍":
			if ( be=="割" || be == "不" || be == "施" || be == "难" || af == "得" || af == "己" || af == "近"){
				a += "捨";
				break;
			}
            else {
            	a += p < 0 ? c : s2.charAt(p);
              	break;
            }
			case "咸":
			if ( af=="阳"){
				a += c;
				break;
			}
            else {
            	a += p < 0 ? c : s2.charAt(p);
              	break;
            }
		    case "云":
			if ( be == "人" || be == "谚" || be == "亦" ){
				a += c;
				break;
			}
            else {
            	a += p < 0 ? c : s2.charAt(p);
              	break;
            }
			case "范":
            if (af=="学" && this.charAt(i+2)=="德"){
              a += c;
              break;
            }
            else {
            	a += p < 0 ? c : s2.charAt(p);
              	break;
            }
			case "冲":
			if ( af=="淡"){
				a += c;
				break;
			}
            else {
            	a += p < 0 ? c : s2.charAt(p);
              	break;
            }
			case "象":
			if ( be=="形" || be == "不" || be == "想" || af=="一" && this.charAt(i+2)=="般" && this.charAt(i+2)=="的"){
				a += "像";
				break;
			}
            else {
            	a += p < 0 ? c : s2.charAt(p);
              	break;
            }
			case "畫":
			if ( be=="计"){
				a += "划";
				break;
			}
            else {
            	a += p < 0 ? c : s2.charAt(p);
              	break;
            }
		  	case "具":
			if ( be=="家"){
				a += "俱";
				break;
			}
            else {
            	a += p < 0 ? c : s2.charAt(p);
              	break;
            }
		  	case "制":
			if ( be=="复" || af == "热" || af == "冷" || af == "作" || af == "造" || af == "片" || af == "品" || af == "表" || af == "人"){
				a += "製";
				break;
			}
            else {
            	a += p < 0 ? c : s2.charAt(p);
              	break;
            }
		  case "著":
			if (af=="作" ||  af=="述" || be=="名" || be=="巨"){
				a += c;
				break;
			}
            else {
            	a += p < 0 ? c : s2.charAt(p);
              	break;
            }
          case "于":
            if (af=="建" && this.charAt(i+2)=="嵘"){
              a += c;
              break;
            }
            else {
            	a += p < 0 ? c : s2.charAt(p);
              	break;
            }
          case "郁":
            if (be=="谢" && this.charAt(i-2)=="文"){
              a += c;
              break;
            }
            else {
            	a += p < 0 ? c : s2.charAt(p);
              	break;
            }
          case "干":
            if (be=="温" && this.charAt(i-2)=="英" || be=="相" || af == "预" || af == "设" || af == "扰"){
              a += c;
              break;
            }
			else if (be=="擦"  || af == "布" || af == "脆" ){
              a += "乾";
              break;
            }
            else {
            	a += p < 0 ? c : s2.charAt(p);
              	break;
            }
		  case "谷":
			if ( be == "山" || be == "低" || be == "填" || be == "曼"){
				a += c;
				break;
			}
            else {
            	a += p < 0 ? c : s2.charAt(p);
              	break;
            }
		  case "准":
			if ( be == "不" || be == "批" || be == "核" || af == "证" || af == "许" || af == "予"){
				a += c;
				break;
			}
            else {
            	a += p < 0 ? c : s2.charAt(p);
              	break;
            }
		  case "布":
			if ( be == "分"|| be == "公" || be == "摆"|| af == "道"){
				a += "佈";
				break;
			}
            else {
            	a += p < 0 ? c : s2.charAt(p);
              	break;
            }
		  case "里":
			if ( af == "程" || be == "公" || be == "海" || be == "英"){
				a += c;
				break;
			}
            else {
            	a += p < 0 ? c : s2.charAt(p);
              	break;
            }
		  case "采":
			if ( af == "访" || af == "取" || be == "开" || af == "用" || af == "样" || af == "摘"){
				a += "採";
				break;
			}
            else {
            	a += p < 0 ? c : s2.charAt(p);
              	break;
            }
		  case "脏":
			if ( af == "器" || be == "心" || be == "胰" || be == "五" || be == "肾" || be == "肝"){
				a += "臟";
				break;
			}
            else {
            	a += p < 0 ? c : s2.charAt(p);
              	break;
            }
		  case "钟":
			if ( af == "鼓" || af == "点" || be == "分" || af == "洪" || be == "警" || be == "晨"){
				a += "鐘";
				break;
			}
            else {
            	a += p < 0 ? c : s2.charAt(p);
              	break;
            }
		  case "尽":
			if ( af == "弃" || af == "美" || af == "忠" || af == "职" || af == "责" || be == "穷" || af == "无" || af == "水"){
				a += "盡";
				break;
			}
			else if ( af == "兴" || af == "管" || be == "早" || af == "量" || be == "快" || be == "力"){
				a += "儘";
				break;
			}
            else {
            	a += p < 0 ? c : s2.charAt(p);
              	break;
            }
		  case "志":
			if ( be == "杂" || be == "标"){
				a += "誌";
				break;
			}
            else {
            	a += p < 0 ? c : s2.charAt(p);
              	break;
            }
		  case "回":
			if ( be == "巡" || be == "轮" || af == "避"){
				a += "迴";
				break;
			}
            else {
            	a += p < 0 ? c : s2.charAt(p);
              	break;
            }
		  case "复":
			if ( af == "婚" || af == "学" || af == "诊" || af == "查" || af == "仇" || af == "活" || af == "兴" || af == "归" || af == "原" || af == "辟" || af == "苏"  || be == "反" || be == "答" || be == "批" || be == "恢" || be == "报" || be == "光" || (be == "不" && this.charAt(i-2)=="劫")){
				a += "復";
				break;
			}
			else if ( af == "庇" || be == "答" || be == "回"){
				a += "覆";
				break;
			}
            else {
            	a += p < 0 ? c : s2.charAt(p);
              	break;
            }
		  case "游":
			if ( be == "下"){
				a += c;
				break;
			}
            else {
            	a += p < 0 ? c : s2.charAt(p);
              	break;
            }
		  case "着":
			if ( af == "手"){
				a += c;
				break;
			}
            else {
            	a += p < 0 ? c : s2.charAt(p);
              	break;
            }
		  case "案":
			if ( be == "方"){
				a += c;
				break;
			}
            else {
            	a += p < 0 ? c : s2.charAt(p);
              	break;
            }
		  case "暴":
			if ( af == "露"){
				a += "曝";
				break;
			}
            else {
            	a += p < 0 ? c : s2.charAt(p);
              	break;
            }
		  case "赞":
			if ( af == "美" || af == "赏" || af == "扬" || af == "指" || af == "指"){
				a += "讚";
				break;
			}
            else {
            	a += p < 0 ? c : s2.charAt(p);
              	break;
            }
          case "系":
			if ( be == "联"){
				a += "繫";
				break;
			}
			else if ( be == "关" || af == "指" ){
				a += "係";
				break;
			}
            else {
            	a += p < 0 ? c : s2.charAt(p);
              	break;
            }
		  case "扎":
			if ( af == "根" || be == "挣"){
				a += c;
				break;
			}
            else {
            	a += p < 0 ? c : s2.charAt(p);
              	break;
            }
		  case "征":
			if ( af == "服"){
				a += c;
				break;
			}
            else {
            	a += p < 0 ? c : s2.charAt(p);
              	break;
            }
		  case "斗":
			if ( af == "篷" || be == "升"){
				a += c;
				break;
			}
            else {
            	a += p < 0 ? c : s2.charAt(p);
              	break;
            }
		  case "发":
			if ( be == "头" ||  be == "毛"){
				a += "髮";
				break;
			}
            else {
            	a += p < 0 ? c : s2.charAt(p);
              	break;
            }
		  case "闆":
			if ( be == "老" ){
				a += "板";
				break;
			}
		  case "板":
			if ( be == "老" ){
				a += "闆";
				break;
			}
		  default:
			a += p < 0 ? c : s2.charAt(p);
		}		
	}
	return a;
}
function setCookie(name, value){
	var argv = setCookie.arguments;
	var argc = setCookie.arguments.length;
	var expires = (argc > 2) ? argv[2] : null;
	if(expires != null){
	   var LargeExpDate = new Date ();
	   LargeExpDate.setTime(LargeExpDate.getTime() + (expires*1000*3600*24));
	}
	document.cookie = name + "=" + escape (value)+((expires == null) ? "" : ("; expires=" +LargeExpDate.toGMTString()));
}
function getCookie(Name){
	var search = Name + "=";
	if(document.cookie.length > 0){
		offset = document.cookie.indexOf(search);
		if(offset != -1){
			offset += search.length;
			end = document.cookie.indexOf(";", offset);
			if(end == -1){
				end = document.cookie.length;
			}
			return unescape(document.cookie.substring(offset, end));
		}else{
			return '';
		}
	}
}
function zh_tranBody(obj){
	var o = (typeof(obj) == "object") ? obj.childNodes : document.body.childNodes;
	for (var i = 0; i < o.length; i++){
		var c = o.item(i);
		if('||BR|HR|TEXTAREA|SCRIPT|'.indexOf("|"+c.tagName+"|") > 0) continue;
		if(c.className == zh_class){
			if(c.id == zh_class + '_' + zh_choose){
				c.setAttribute('style', zh_style_active);
				c.style.cssText = zh_style_active;
			}else{
				c.setAttribute('style', zh_style_inactive);
				c.style.cssText = zh_style_inactive;
			}
			continue;   
		}
		if(c.title != '' && c.title != null){
			c.title = c.title.tran();
		}
		if(c.alt != '' && c.alt != null){
			c.alt = c.alt.tran();
		}
		if(c.tagName == "INPUT" && c.value != '' && c.type != 'text' && c.type != 'hidden' && c.type != 'password'){
			c.value = c.value.tran();
		}
		if(c.nodeType == 3){
			c.data = c.data.tran();  
		}else{
			zh_tranBody(c);
		}
	}
}
function zh_tran(go){
	if(go) zh_choose = go;
	setCookie('zh_choose', zh_choose, zh_expires);
	if(go == 'n'){
	   window.location.reload();
	}else {
	   zh_tranBody();
	}
    if (zh_choose == "s") {jQuery('#h-logo').attr('class','sim');}
	if (zh_choose == "t") {jQuery('#h-logo').attr('class','tra');}
	
}
function zh_getLang(){
	if(getCookie('zh_choose')){
	   zh_choose = getCookie('zh_choose');
	   return true;
	}
	if(!zh_autoLang_t && !zh_autoLang_s){
		return false;
	}
	if(getCookie('zh_autoLang_checked')){
		return false;
	}
	if(navigator.language){
		zh_browserLang = navigator.language;
	}else if(navigator.browserLanguage){
		zh_browserLang = navigator.browserLanguage;
	}
	if(zh_autoLang_t && zh_langReg_t.test(zh_browserLang)){
		zh_choose = 't';
	}else if(zh_autoLang_s && zh_langReg_s.test(zh_browserLang)){
		zh_choose = 's';
	}
	zh_autoLang_checked = 1;
	setCookie('zh_choose', zh_choose, zh_expires);
	if(zh_choose == zh_default){
		return false;
	}
	return true;
}
function zh_init(){
	zh_getLang();
	c = document.getElementById(zh_class + '_' + zh_choose);
	if(zh_choose != zh_default){
		if(window.onload){
			window.onload_before_zh_init = window.onload;
			window.onload = function(){
				zh_tran(zh_choose);
				if(zh_autoLang_alert){
					alert(zh_autoLang_msg);
				};
				window.onload_before_zh_init();
			};
		}else{
			window.onload = function(){
				zh_tran(zh_choose);
				if(zh_autoLang_alert){
					alert(zh_autoLang_msg);
				};
			};
		}
	}
}
zh_init();
