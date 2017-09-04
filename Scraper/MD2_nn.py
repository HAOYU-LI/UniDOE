import scraper

MD2_nn_url = "http://www.cms-ud.com/UD/table/Un_n%5Es_MD.html"; crit_name_MD2_nn = "Un_n\^"
MD2_nn_list = find_urls(url=MD2_nn_url,crit_name=crit_name_MD2_nn)
for url in MD2_nn_list:
    try:
        url = url.replace('^','%5E')
        url = "http://www.cms-ud.com/UD/table/"+url
        design = find_design(url)
        #print(design)
        run,factor,level = find_design_size(url,"MD2_nn")
        file_name = "MD2_"+str(run)+"_"+str(factor)+"_"+str(level)
        save_design(design,name=file_name,save_path='./design_data/MD2_nn/')
    except:
        print("Some errors happen at url: %s",url)