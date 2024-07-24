package lettercode.Easy

import javax.swing.tree.TreeNode

object Evaluate_Boolean_Binary_Tree {

  /**
   * Definition for a binary tree node.
   * class TreeNode(_value: Int = 0, _left: TreeNode = null, _right: TreeNode = null) {
   *   var value: Int = _value
   *   var left: TreeNode = _left
   *   var right: TreeNode = _right
   * }
   */
  object Solution {
    def evaluateTree(root: TreeNode): Boolean = {
      root.value match {
        case 0 => false
        case 1 => true
        case 2 => evaluateTree(root.left) || evaluateTree(root.right)
        case 3 => evaluateTree(root.left) && evaluateTree(root.right)
      }
    }
  }

}
