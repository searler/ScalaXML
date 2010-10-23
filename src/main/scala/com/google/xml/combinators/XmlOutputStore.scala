package com.google.xml.combinators


import scala.collection._



import org.w3c.dom._

import javax.xml.parsers.{DocumentBuilderFactory,DocumentBuilder}

/**
 * An XML store used during pickling. It provides methods for adding
 * XML elements, attributes and namespaces. Implementers decide on the
 * actual strategy for looking up elements based on name (linear or 
 * random access).
 * 
 * @author Iulian Dragos
 */
trait XmlOutputStore{
  

  /** Return a new XmlStore with a new attribute prepended to the list of attrs */
  def addAttribute(pre: String, uri:String, key: String, value: String): XmlOutputStore
  
  /** Return a new XmlStore with an unprefixed attribute appended to the list of attrs. */
  def addAttribute(key: String, value: String): XmlOutputStore
   
  def text:String

  /** Add a text node */
  def addText(s: String): XmlOutputStore 

  def addNodes(ns: Seq[Node]): XmlOutputStore =
//    ns.foldLeft(this) (_.addNode(_))
  this
  
  /** Add a node. */
  def addNode(prefix: String, uri:String,label: String): XmlOutputStore

  /** Add an entire XmlStore to this store. */
  def addStore(other: XmlStore): XmlOutputStore

  def document:Document
}

/**
 * A PlainOutputStore implements XmlOutputStore with reasonable efficiency. It
 * is a mutable representation.
 */
class PlainOutputStore(val node:Node) extends XmlOutputStore {

  def document = node match {
        case d:Document => d
        case _  => node getOwnerDocument
   }
   def element = node.asInstanceOf[Element]

/** Add a text node */
  def addText(s: String): XmlOutputStore = {
    node.setTextContent(s)
    this
  }
  
  /** Return a new LinearStore with a prefixed attribute prepended to the list of attrs */
  def addAttribute(pre: String, uri:String,key: String, value: String): XmlOutputStore = {
    element.setAttributeNS(uri,pre+":"+key,value)
    this
  }

  /** Return a new LinearStore with an unprefixed attribute prepended to the list of attrs */
  def addAttribute(key: String, value: String): XmlOutputStore = {
    element.setAttribute(key,value)
    this
  }
    
  def text:String = document.toString + node.toString

  

  def addNode(prefix: String, uri:String, label: String) = {
   
    val e = document.createElementNS(uri, prefix+":"+label)
    node.appendChild(e)
    new PlainOutputStore(e)
   
  }
  
 

  /** Add an entire XmlStore to this store. */
  def addStore(other: XmlStore): XmlOutputStore = {
   // val newAttrs = attrs 
  //  other.attrs.foreach(attrs.append(_)) ##
  
    this
  } 
}

/** Factory for output stores. */
object PlainOutputStore {

   val builder = DocumentBuilderFactory.newInstance.newDocumentBuilder 
  /** An empty output store. */
  def empty: PlainOutputStore = new PlainOutputStore(builder.newDocument)
  
  /** An output store with a given namespace binding. */
  def apply() = empty
}

/** An exception thrown when the XML output store is inconsistent. */
case class MalformedXmlStore(msg: String, state: XmlOutputStore) extends RuntimeException(msg)
