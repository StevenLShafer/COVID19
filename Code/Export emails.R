library(RDCOMClient)

##################
OutApp <- COMCreate("Outlook.Application")
OutStores <- OutApp$Session()$Stores()
OutApp$GetNameSpace("MAPI")

counts <- OutStores$Count()
RootFolders <- NULL
for (i in 1:counts)
  RootFolders <- c(RootFolders, OutStores[[i]]$GetRootFolder()$FolderPath())
X <- which(RootFolders == "\\\\steveshafer@gmail.com")

gmail <- OutStores[[X]]$GetRootFolder()$Folders()
counts <- gmail$Count()
counts
GmailFolders <- NULL
for (i in 1:counts)
  GmailFolders <- c(GmailFolders, gmail[[i]]$FolderPath())

Inbox <- which(GmailFolders == "\\\\steveshafer@gmail.com\\Inbox")
gmail[[Inbox]]$Items()$count()
gmail[[Inbox]]$Items
gmail[[Inbox]]$folders()

GmailFolders

myfolder <- OutStores[[1]]$GetRootFolder()$folders()

for (i in 1:store_count) {
  myfolder <- OutStores[[1]]$GetRootFolder()$folders("Inbox")
  
  emails <- myfolder$Items
  
  for (i in 1:10) {
    subject <- emails(i)$Subject()
    print(subject) 
  }
}


outlook_app <- COMCreate("Outlook.Application")
outlookNameSpace <- outlook_app$GetNameSpace("MAPI")




outlookNameSpace$Folders(6)$Folders(2)$Name(1)
search <- outlook_app$AdvancedSearch(
  outlookNameSpace$Folders(6)$Folders(2),
  "urn:schemas:httpmail:subject = 'Daily COVID Update for Tuesday, September 22, 2020'"
)


search <- outlook_app$AdvancedSearch(
  "Inbox - steveshafer@gmail.com",
  "urn:schemas:httpmail:subject = 'Daily COVID Update for Tuesday, September 22, 2020'"
)
results <- search$Results()
results$Count()


# QUIT APPLICATION
OutApp$Quit()

# RELEASE COM RESOURCES
subject <- NULL; emails <- NULL; myfolder <- NULL
OutStores <- NULL; OutApp <- NULL
gc()