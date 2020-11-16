# Send email
sendEmail <- function()
{
  emailText1 <- paste(
    emailText,
    email.list.start,
    paste("Yada Yada Yada"),
    email.list.end,
    paste(
      readLines(paste0(dirCode,"email.body.end.txt")),
      collapse = "\n"
    )
  )
  send.mail(
    from = "stanpumpR@gmail.com",
    to = "steveshafer@gmail.com",
    subject = paste("Daily COVID Update for", format(today, "%A, %B %d, %Y")),
    body = emailText1,
    html = TRUE,
    smtp = list(
      host.name = "smtp.gmail.com",
      port = 465,
      user.name = config::get("email_username"),
      passwd = config::get("email_password"),
      ssl = TRUE),
    authenticate = TRUE
  )
}
