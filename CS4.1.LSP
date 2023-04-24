;------------------------------------------------------------------------------
; 文件名：CS4.lsp
; 作者：DeQiang Kong
; 邮箱：2062713105@qq.com
; 日期：2023-04-23
; 描述：自定义标注样式的坐标标注程序（带三角形标注符号的标注方法）
; 备注：默认标注格式使用宋体做模板，其他字体会偏移不居中,通过更改 HVRatio与VOffset 的值来可以以调节横向纵向标注位置
; 备注：脚本标注的字体设置通过CAD内置的 STYLE 功能更改，脚本自身不支持字体修改
;------------------------------------------------------------------------------

(defun c:CS ()
	(setq HVRatio 0.8);表示点个字体的横纵比例 *** 字宽=字高*0.8 ***
  	(setq VOffset 0.16);表示标注位置相对于三角顶面的偏移位置*** +为向上 -为向下 ***
  	(setq sideLength 0.1536);等边三角形边长
	(setq pt (getpoint "\n选择要标注的点: "))
  	(setq formatted-Y (cadr pt))
  	(setq formatted-X (car pt))
	(setq rotation-angle 0.0) 
	(setq angleRad (/ pi 3.0))
	(setq pt1 (polar pt angleRad sideLength))
	(setq angleRad (+ angleRad (/ pi 3.0)))
	(setq pt2 (polar pt angleRad sideLength))
	;;
	(setq radius (/ (getvar 'dimasz) 2.0))
	(setq offset (* 0.1 radius))
	(setq textPt (list (car pt)(+ (cadr pt) offset)))
	(setq text-style (getvar "TEXTSTYLE")) 
	;;(PRINC text-style )
	(setq height (cdr (assoc 40 (tblsearch "STYLE" text-style))))
	(setq width-factor (cdr (assoc 41 (tblsearch "STYLE" text-style))))
	(cond
		((= height 0)(princ "错误!@! 字体高度不能为0。请使用 style 命令修改文字大小以及字体样式")) 
		(T
        	(setq formatted-Y (cond
                               ((=(- (cadr pt) (fix(cadr pt))) 0) (strcat (rtos (cadr pt) 2 0) ".000"))
                               (T
									(setq formatted-Y
                                        (cond
											((= (rtos (cadr pt) 2 3) 0)(strcat (rtos (cadr pt) 2 0) ".000"))
											(T 
                                              	(setq num-str (rtos (cadr pt) 2 3)) 
												(setq dot-pos (vl-string-search "." num-str))
												(if dot-pos
													(progn
														(setq integer-part (substr num-str 1 dot-pos))
														(setq decimal-part (substr num-str (+ dot-pos 2))) 											
															(strcat (strcat integer-part ".")
																	(COND
																		((= (STRLEN decimal-part) 1)(setq decimal-part(strcat decimal-part "00")))
																		((= (STRLEN decimal-part) 2)(setq decimal-part (strcat decimal-part "0")))
																		(T(setq decimal-part decimal-part ))
																	) 
															)													
													)
													(progn
														(strcat (strcat num-str ".") "000")
													)			
												)                                            
                                            )
                                    	)
                                    )
                               )
                              ) 
            )
        	(setq formatted-X (cond
                               ((=(- (car pt) (fix(car pt))) 0) (strcat (rtos (car pt) 2 0) ".00"))
                               (T
									(setq formatted-X
                                 		(cond
											((= (rtos (car pt) 2 2) 0)(strcat (rtos (car pt) 2 0) ".00"))
											(T 
                                            	(setq num-str2 (rtos (car pt) 2 2))
												(setq dot-pos2 (vl-string-search "." num-str2))
												(if dot-pos2
													(progn
														(setq integer-part2 (substr num-str2 1 dot-pos2)) 
														(setq decimal-part2 (substr num-str2 (+ dot-pos2 2))) 
														
															(strcat (strcat integer-part2 ".")
																(COND
																	((= (STRLEN decimal-part2) 1)(setq decimal-part2(strcat decimal-part2 "0")))
																	(T(setq decimal-part2 decimal-part2 ))
																)
															)
														
														(strcat (strcat integer-part2 ".") decimal-part2)
													)
													(progn
														(strcat (strcat num-str2 ".") "00")
													)		
												)
                                            
                                            )
										)
									)
                               )
                              ) 
            )		
			(setq resulttext (strcat formatted-X "," formatted-Y)) ;
			(setq text-width (* (strlen resulttext) height )) 
        	(cond 
           		((= width-factor 1) (setq text-width (* text-width (* width-factor HVRatio ))))
           		(T (setq text-width (* text-width (* width-factor HVRatio ))))
            )
			(setq movex (* 0.5 text-width))
			(setq text-point (list (- (car pt) (* 0.5 text-width)) (+ (cadr pt) VOffset))) ; 
			(command "_.pline" pt pt1 pt2 "c")
			(cond
				((= text-style "Standard")(command "_text" text-point rotation-angle resulttext))
				((= text-style "宋体") (command "_text" text-point rotation-angle resulttext))
				(T (command "_text" text-point rotation-angle resulttext))
			)
		)	
	) 
)

