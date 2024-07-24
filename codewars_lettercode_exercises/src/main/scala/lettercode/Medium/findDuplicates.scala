package lettercode.Medium

object findDuplicates {

  def findDuplicate(paths: Array[String]): List[List[String]] = {
    paths
      .flatMap((pathAndFiles: String) => {
        val (path: String) :: (filesAndContents: List[String]) = pathAndFiles.split(" ").toList
        filesAndContents.map((fileAndContent: String) => {
          val startIndex = fileAndContent.indexOf("(")
          val fileName: String = fileAndContent.substring(0, startIndex)
          val content = fileAndContent.substring(startIndex + 1, fileAndContent.length - 1)
          content -> s"$path/$fileName"
        })
      }) //return pairs (content, fullFilePath)
      .groupBy(_._1) //group by content
      .view
      .mapValues(_.map(_._2).toList) //for each content item map over the fullFilePaths as a list
      .values //we can throw away the key
      .filter(_.length > 1) //we only care about duplicates
      .toList
  }
}
