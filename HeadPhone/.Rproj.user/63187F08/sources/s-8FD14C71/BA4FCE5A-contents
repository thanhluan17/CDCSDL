#install library
install.packages('xml2')
install.packages('rvest')
install.packages("writexl")
install.packages('selectr')
install.packages("xlsx")
install.packages("magrittr")
install.packages("dplyr")
install.packages("tidyverse")
install.packages("rebus")
install.packages("lubridate")


#loading the package
library(xml2)
library(rvest)
library(writexl)
library(stringr)
library(xlsx)
library(magrittr)
library(dplyr)
library(tidyverse)
library(rebus)     
library(lubridate)


url <- 'https://tainghe.com.vn/tai-nghe.html'
homepage <- read_html(url)

#lay du lieu cho bang thuong hieu
#lay danh sach cac thuong hieu
brand_html <- html_nodes(homepage, xpath="/html/body/div[3]/div[5]/div[1]/div[4]/div[1]/ul[2]/li/a")
brands <- html_text(brand_html)
brandName <-c()
brandId <-c()
ThuongHieu <- data.frame(brandId, brandName)
i<-1
for (name in brands){
  #lay ten nhan hieu
  brandName <-c(name)
  #lay ma nhan hieu
  brandId <- c(str_c("BRAND",i))
  temp <- data.frame(brandId, brandName)
  ThuongHieu <- rbind(ThuongHieu,temp)
  i<-i+1
}


#lay du lieu cho bang loai san pham
#lay danh sach cac loai san pham
category_html<-html_nodes(homepage, xpath="/html/body/div[3]/div[5]/div[1]/div[4]/div[1]/ul[1]/li/a")
categories_url<-html_attr(category_html,"href")
categories_url<-str_c("https://tainghe.com.vn",categories_url)

category_name<-html_text(category_html)
category_name<-str_trim(category_name)

categoryId <-c()
categoryName <-c()
categoryHref <-c()
LoaiSanPham <- data.frame(categoryId, categoryName, categoryHref)
i<-1
for (name in category_name){
  #lay ma loai san pham
  categoryId <- c(str_c("TYPE",i))
  #lay ten loai san pham
  categoryName <-c(name)
  #lay duong dan loai san pham
  categoryHref <-c(categories_url[i])
  temp <- data.frame(categoryId, categoryName, categoryHref)
  LoaiSanPham <- rbind(LoaiSanPham,temp)
  i<-i+1
}


productId <- c()
productName <- c()
productPrice <- c()
productImage <- c()
categoryHref <-c()

SanPham <- data.frame(productId, productName, productPrice, productImage, categoryHref)
i<-1

for(category_url in categories_url){
  if(str_starts(category_url,"https://tainghe.com.vn/")== FALSE){
    category_url<- str_c("https://tainghe.com.vn",category_url)
  }
  if(str_starts(category_url,"https://tainghe.com.vn")){
    category_page<-read_html(category_url)
    
    products_html<-html_nodes(category_page,".item div")
    products_detail_url<-html_nodes(products_html,"a")
    products_detail_url<-html_attr(products_detail_url,"href")
    products_detail_url<- str_c("https://tainghe.com.vn",products_detail_url)
    for(product_detail_url in products_detail_url){
      product_detail_page<-read_html(product_detail_url)
      
      #lay ten san pham
      product_name <- html_text(html_node(product_detail_page,"#detail-name"))
      productName <- c(product_name)
      #duong dan anh cua san pham
      product_img <- html_attr(html_node(product_detail_page,".z-fea img"),"src")
      productImage <- c(product_img)
      productImage <-c(str_c("https://tainghe.com.vn/",productImage))
      
      #gia ban cua san pham
      product_final_price<-html_text(html_node(product_detail_page,".price_config"))
      #loai bo don vi 'đ'
      product_final_price<-str_sub(product_final_price,0,str_length(product_final_price)-2)
      if(is.na(product_final_price)){
        product_final_price<-0
      }else{
        #loai bo dau cham
        productPrice <- c(product_final_price)
        productPrice <- str_replace_all(productPrice, "[[:punct:]]" , "")
      }
      
      #lay ma san pham
      productId <- c(str_c("PRODUCT",i))
      #lay duong link cua danh muc
      categoryHref <- c(category_url)
      temp <- data.frame(productId, productName, productPrice, productImage, categoryHref)
      SanPham <- rbind(SanPham,temp)
      i<-i+1
    }
  }
}

#Join với DanhMuc để tìm mã DanhMuc
SanPham.lookup <- data.frame(LoaiSanPham[3],LoaiSanPham[1])
SanPham<-merge(SanPham, SanPham.lookup)
SanPham <- SanPham %>% select(productId, productName, productPrice, productImage, categoryId)
LoaiSanPham <- LoaiSanPham %>% select(categoryId:categoryName)


write.xlsx(LoaiSanPham,file= "D:\\HOCTAP\\220\\ChuyenDeCSDL\\R\\HeadPhone\\ScrappedData.xlsx",sheetName="LoaiSanPham",append=TRUE)
write.xlsx(NhanHieu,file= "D:\\HOCTAP\\220\\ChuyenDeCSDL\\R\\HeadPhone\\ScrappedData.xlsx",sheetName="NhanHieu",append=TRUE)
write.xlsx(SanPham,file= "D:\\HOCTAP\\220\\ChuyenDeCSDL\\R\\HeadPhone\\ScrappedData.xlsx",sheetName="SanPham",append=TRUE)


