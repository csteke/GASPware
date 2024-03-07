nedit.highlightPatterns: GSORT:1:0{\n\
                Comment:"^[*]":"$"::Comment::\n\
                Formats :"<(?igasp|gasp2001|euroball|gammasphere|yale|tandem|presorted|prisma)>":::Keyword::D\n\
                Types :"^[ \\t]*<(?iformat|header|detector|cdetector|rawfoldmin|hgatedef|pairdef)>":::Flag::D\n\
                Keywords 1:"^[ \\t]*<(?ifold|gate|gates|window|banana|bananas|filter|xban|pin|select|pairgate|pairkill)>":::Keyword::D\n\
                Keywords 2:"^[ \\t]*<(?irecal|gain|recal_mult|recal_lut|recal_doppler|recal_dopp|recal_kine|recal_choose|recal_polar)>":::Keyword::D\n\
                Keywords 3:"^[ \\t]*<(?ihk|add|addback|mult|power|divide|div|mean_value|wmean_value|mean|wmean|copy|move|kill|ebkill|newid|reorder|sta|stat|stati|statis|statist|statisti|statistic|statistics)>":::Keyword::D\n\
                Keywords 4:"^[ \\t]*<(?irecall|recall_e|recall_event|store|store_e|store_event|list|list_event|list_e|break)>":::Keyword::D\n\
                Keywords 5:"^[ \\t]*<(?itime_ref|time_reference|time_r|time_a|time_adj|time_adjust|timing|comb|combine|split|swap|mask|esl_to_ecm)>":::Keyword::D\n\
                Keywords 6:"^[ \\t]*<(?iproje|project|projection|projections|sort1d|sort1d_m|sort2d|sort2d_symm|sort2d_hsymm|sort2d_ab|sort3d|sort3d_symm|sort3d_hsymm|sort3d_pair|sort3d_diff|sort4d|sort4d_symm|sort4d_hsymm)>":::Keyword::D\n\
                Keywords 7:"^[ \\t]*<(?iusersub)([1-9]+)>":::Keyword::D\n\
		Keywords 8:"^[ \\t]*<(?itrack_p|track_pr|track_pri|track_pris|track_prism|track_prisma)>":::Keyword::D\n\
		Keywords 9:"^[ \\t]*<(?iqvalue_p|qvalue_pr|qvalue_pri|qvalue_pris|qvalue_prism|qvalue_prisma)>":::Keyword::D\n\
		Keywords 10:"^[ \\t]*<(?iangles_p|angles_pr|angles_pri|angles_pris|angles_prism|angles_prisma)>":::Keyword::D\n\
		Keywords 11:"^[ \\t]*<(?ithr|thre|thres|thresh|thresho|threshol|threshold|thresholds)>":::Keyword::D\n\
		Keywords 12:"^[ \\t]*<(?ibp_vel|bp_velo|bp_veloc|bp_veloci|bp_velocit|bp_velocity|bp_velocity_p|bp_velocity_prisma)>":::Keyword::D\n\
		Detector specifiers :"<(?iplus|seg)>":::String::D\n\
		Specifiers :"[ \\t]+<(?iclara|run|norun|in|out|gain|offset|off|fact|factor|factors|res|step|always|ifvalid|zero|reorder|integer)>":::String::D\n\
		Data id   :"[ \\t]+<(?i[a-z])>[ \\t\\n]+":::Storage Type::\n\
		Data par  :"[ \\t]+<(?i[a-z])([0-9]+)>":::Storage Type::\n\
      }
nedit.languageModes:    GSORT:.setup::::::
