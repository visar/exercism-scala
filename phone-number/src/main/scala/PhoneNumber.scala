object PhoneNumber {
  def clean(input: String): Option[String] = {
    if ((input intersect (('a' to 'z') ++ "@:!")).nonEmpty)
      None
    else {
      val cleanNumber = input.filter(('0' to '9').contains(_))
      if (cleanNumber.length == 11 && cleanNumber(0) == '1')
        Some(cleanNumber.slice(1, cleanNumber.length))
      else if (cleanNumber.length != 10)
        None
      //  Area code
      else if (cleanNumber(0) == '0' || cleanNumber(0) == '1')
        None
      //  Exchange code
      else if (cleanNumber(3) == '0' || cleanNumber(3) == '1')
        None
      else
        Some(cleanNumber)
    }
  }
}
