from scraper import *

WD2_url = "http://www.cms-ud.com/UD/table/WD2.htm"; crit_name_WD2 = "WD2/"
WD2_list = find_urls(url=WD2_url,crit_name=crit_name_WD2)
for url in WD2_list:
    try:
        url = url.replace('^','%5E')
        url = "http://www.cms-ud.com/UD/table/"+url
        design = find_design(url)
        #print(design)
        run,factor,level = find_design_size(url,"WD2")
        file_name = "WD2_"+str(run)+"_"+str(factor)+"_"+str(level)
        save_design(design,name=file_name,save_path='./design_data/WD2/')
    except:
        print("Some errors happen at url: %s",url)