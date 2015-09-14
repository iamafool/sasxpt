(in-package :sasxpt)

(let ((n-recs (list
               (namestr-data-instance 1 "STUDYID" "Char" 7  "$" "$" "Study Identifier")
               (namestr-data-instance 2 "DOMAIN" "Char" 2 "$" "$" "Domain Abbreviation")
               (namestr-data-instance 3 "USUBJID" "Char" 14 "$" "$" "Unique Subject Identifier")
               (namestr-data-instance 4 "SUBJID" "Char" 6 "$" "$" "Subject Identifier for the Study")
               (namestr-data-instance 5 "RFSTDTC" "Char" 10 "$" "$" "Subject Reference Start Date/Time")
               (namestr-data-instance 6 "RFENDTC" "Char" 10 "$" "$" "Subject Reference End Date/Time")
               (namestr-data-instance 7 "SITEID" "Char" 3 "$" "$" "Study Site Identifier")
               (namestr-data-instance 8 "BRTHDTC" "Char" 10 "$" "$" "Date/Time of Birth")
               (namestr-data-instance 9 "AGE" "Num" 8 " " " " "Age")
               (namestr-data-instance 10 "AGEU" "Char" 5 "$" "$" "Age Units")
               (namestr-data-instance 11 "SEX" "Char" 1 "$" "$" "Sex")
               (namestr-data-instance 12 "RACE" "Char" 40 "$" "$" "Race")
               (namestr-data-instance 13 "ETHNIC" "Char" 22 "$" "$" "Ethnicity")
               (namestr-data-instance 14 "ARMCD" "Char" 8 "$" "$" "Planned Arm Code")
               (namestr-data-instance 15 "ARM" "Char" 20 "$" "" "Description of Planned Arm")
               (namestr-data-instance 16 "COUNTRY" "Char" 3 "$" "$" "Country")
               ))
      (o-recs '(("CDISC01" "DM" "CDISC01.100008" "100008" "2003-04-29" "2003-10-12" "100"
                 "1930-08-05" 72.0 "YEARS" "M" "OTHER                                   "
                 "NOT HISPANIC OR LATINO" "WONDER10" "Miracle Drug 10 mg  " "USA")
                ("CDISC01" "DM" "CDISC01.100014" "100014" "2003-10-15" "2004-03-29" "100"
                 "1936-11-01" 66.0 "YEARS" "F" "WHITE                                   "
                 "NOT HISPANIC OR LATINO" "WONDER20" "Miracle Drug 20 mg  " "USA")
                ("CDISC01" "DM" "CDISC01.200001" "200001" "2003-09-30" "2004-02-02" "200"
                 "1923-09-03" 80.0 "YEARS" "F" "MULTIPLE                                "
                 "NOT HISPANIC OR LATINO" "PLACEBO " "Placebo             " "USA")
                ("CDISC01" "DM" "CDISC01.200002" "200002" "2003-10-10" "2004-03-28" "200"
                 "1933-07-22" 70.0 "YEARS" "F" "BLACK OR AFRICAN AMERICAN               "
                 "NOT HISPANIC OR LATINO" "WONDER10" "Miracle Drug 10 mg  " "USA")
                ("CDISC01" "DM" "CDISC01.200005" "200005" "          " "          " "200"
                 "1937-02-22" 66.0 "YEARS" "F" "WHITE                                   "
                 "NOT HISPANIC OR LATINO" "SCRNFAIL" "Screen Failure      " "USA"))))
  (adjust-npos n-recs)
  (write-xpt "dm01.xpt"  "DM01" "Demographics" n-recs o-recs))


(let ((n-recs (list
               (namestr-data-instance 1 "NAME"   "Char" 12 "$" "$" "����")
               (namestr-data-instance 2 "SEX"    "Char" 4  "$" "$" "�Ա�")
               (namestr-data-instance 3 "AGE"    "Num"  8  " " " " "����")
               (namestr-data-instance 4 "HEIGHT" "Num"  8  " " " " "��ߣ�Ӣ�磩")
               (namestr-data-instance 5 "WEIGHT" "Num"  8  " " " " "���أ�����")
               ))
      (o-recs '(("�������׵�" "��" 14.0 69.0 112.5)
                ("����˿" "Ů" 13.0 56.5 84.0)
                ("�Ű���" "Ů" 13.0 65.3 98.0)
                ("��¶" "Ů" 14.0 62.8 102.5)
                ("����" "��" 14.0 63.5 102.5)
                ("ղķ˹" "��" 12.0 57.3 83.0)
                ("��" "Ů" 12.0 59.8 84.5)
                ("������" "Ů" 15.0 62.5 112.5)
                ("�ܸ���" "��" 13.0 62.5 84.0)
                ("Լ��" "��" 12.0 59.0 99.5)
                ("����˹" "Ů" 11.0 51.3 50.5)
                ("���" "Ů" 14.0 64.3 90.0)
                ("����˹" "Ů" 12.0 56.3 77.0)
                ("����" "Ů" 15.0 66.5 112.0)
                ("������" "��" 16.0 72.0 150.0)
                ("�޲���" "��" 12.0 64.8 128.0)
                ("���ɵ�" "��" 15.0 67.0 133.0)
                ("����˹" "��" 11.0 57.5 85.0)
                ("����" "��" 15.0 66.5 112.0))))
  (setf *sas-xpt-ef* :gbk)
  (adjust-npos n-recs)
  (write-xpt "class01.xpt"  "CLASS01" "ѧ������" n-recs o-recs))

