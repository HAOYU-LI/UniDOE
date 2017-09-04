import scraper

CL2_nn_url = "http://www.cms-ud.com/UD/table/Un_n%5Es.html"; crit_name_Cl2_nn = "Un_n\^"
CL2_nn_list = find_urls(url=CL2_nn_url,crit_name=crit_name_Cl2_nn)
for url in CL2_nn_list:
    try:
        url = url.replace('^','%5E')
        url = "http://www.cms-ud.com/UD/table/"+url
        design = find_design(url)
        #print(design)
        run,factor,level = find_design_size(url,"CL2_nn")
        file_name = "CL2_"+str(run)+"_"+str(factor)+"_"+str(level)
        save_design(design,name=file_name,save_path='./design_data/')
    except:
        print("Some errors happen at url: %s",url)