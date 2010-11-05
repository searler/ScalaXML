package com.google.xml.combinators


import org.w3c.dom._

object NullNamedNodeMap extends NamedNodeMap {
  def getLength:Int  = 0
  def getNamedItem(name:String) = null
  def getNamedItemNS(uri:String,local:String) = null
  def item(index:Int) = null
  def removeNamedItem(name:String) = null
  def removeNamedItemNS(uri:String,name:String) = null
  def setNamedItem(arg:Node) = arg
  def setNamedItemNS(arg:Node) = arg
}