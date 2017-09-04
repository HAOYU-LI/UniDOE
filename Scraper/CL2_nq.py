import scraper

CL2_nq_url = "http://www.cms-ud.com/UD/table/CD2.htm"; crit_name_Cl2_nq = "CD2/"
CL2_nq_list = find_urls(url=CL2_nq_url,crit_name=crit_name_Cl2_nq)
for url in CL2_nq_list:
    try:
        url = url.replace('^','%5E')
        url = "http://www.cms-ud.com/UD/table/"+url
        design = find_design(url)
        #print(design)
        run,factor,level = find_design_size(url,"CL2_nq")
        file_name = "CL2_"+str(run)+"_"+str(factor)+"_"+str(level)
        save_design(design,name=file_name,save_path='./design_data/CL2_nq/')
    except:
        print("Some errors happen at url: %s",url)