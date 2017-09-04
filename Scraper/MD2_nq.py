import scraper

MD2_nq_url = "http://www.cms-ud.com/UD/table/MD2.htm"; crit_name_MD2_nq = "MD2/"
MD2_nq_list = find_urls(url=MD2_nq_url,crit_name=crit_name_MD2_nq)
for url in MD2_nq_list:
    try:
        url = url.replace('^','%5E')
        url = "http://www.cms-ud.com/UD/table/"+url
        design = find_design(url)
        #print(design)
        run,factor,level = find_design_size(url,"MD2_nq")
        file_name = "MD2_"+str(run)+"_"+str(factor)+"_"+str(level)
        save_design(design,name=file_name,save_path='./design_data/MD2_nq/')
    except:
        print("Some errors happen at url: %s",url)