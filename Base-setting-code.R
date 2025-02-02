# Rmd 파일 저장 후 돌리는 code

# jekyll 블로그 디렉토리 설정
base <- "/Users/bong/Library/Mobile Documents/com~apple~CloudDocs/Thesis/bong6780.github.io/"

# Rmd 파일이 저장된 디렉토리 지정
rmds <- "_Rmd"
setwd(base)

# 파일 이름 지정
filename <- "2020-10-12-test.Rmd"

# 폴더 경로들
figs.path <- "assets/article_images/"
posts.path <- "_posts/R/"

# Rmd -> md 변환
require(knitr)
render_jekyll(highlight = "pygments")
opts_knit$set(base.url="/")

file <- paste0(rmds, "/", filename)

### 파일 경로 설정
fig.path <- paste0(figs.path, sub(".Rmd$", "", basename(file)), "/")
opts_chunk$set(fig.path = fig.path)

### suppress messages
opts_chunk$set(cache = F, warning = F, message = F, cache.path = "_cache/", tidy = F)

### 파일 변환 및 경로 지정
out.file <- basename(knit(file))
file.rename(out.file, paste0(posts.path, out.file))
