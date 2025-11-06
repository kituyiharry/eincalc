<script>
  import { Canvas, Layer } from 'svelte-canvas';
  // TODO: standardize controller so that we can type it
  import { default as controller } from '$lib/index';
  import { onMount } from 'svelte';

  // editing support via floating input 
  let editor
  let plotArea 
  let canvas

  onMount(() => {
    controller.myLib.renderarea(plotArea);

    resizeCanvas();
    window.addEventListener('resize', resizeCanvas);
    window.addEventListener('mouseup', handleMouseUp);

    return () => {
      window.removeEventListener('resize', resizeCanvas);
      window.removeEventListener('mouseup', handleMouseUp);
    };

  })

  let refresh = $state(0);

  const CELL_WIDTH         = 120;
  const CELL_HEIGHT        = 30;
  const HEADER_HEIGHT      = 30;
  const ROW_HEADER_WIDTH   = 60;
  const RESIZE_HANDLE_SIZE = 4;

  // needed to render the editing cell absolutelt
  const DRAWER           = 57; 

  // Scroll and Viewport computation
  let scrollX =  $state(0);
  let scrollY =  $state(0);
  let cellData = $state({})
  let cellStyles = {}; // Stores style information for each cell

  // a place to hold changes
  let styleBuffer = $state({
    backgroundColor: '#ffffff',
    color: '#000000',
    fontWeight: 'normal',
    fontStyle: 'normal',
    textDecoration: 'none',
    fontSize: 12,
    textAlign: 'left',
    verticalAlign: 'middle',
    borderColor: '#ddd',
    borderWidth: 1
  })

  // Default style object structure
  const defaultStyle = {
    backgroundColor: '#ffffff',
    color: '#000000',
    fontWeight: 'normal',
    fontStyle: 'normal',
    textDecoration: 'none',
    fontSize: 12,
    textAlign: 'left',
    verticalAlign: 'middle',
    borderColor: '#ddd',
    borderWidth: 1
  };

  // Default style object structure
  const selectStyle = {
    borderColor:      '#1976d2',
    borderWidth:       1,
    foregroundColor:  '#cce5ff',
  };

   // Get style for a specific cell
  function getCellStyle(row, col) {
    const key = `${row},${col}`;
    return cellStyles[key] || { ...defaultStyle };
  }

  // Set style for a specific cell
  function setCellStyle(row, col, styleProps) {
    const key = `${row},${col}`;
    cellStyles[key] = {
      ...getCellStyle(row, col),
      ...styleProps
    };
    //drawGrid();
    refresh++;
  }

  // Apply style to current selection
  function applyStyleToSelection(styleProps) {
    const bounds = getSelectionBounds();
    if (!bounds) return;
    
    for (let row = bounds.startRow; row <= bounds.endRow; row++) {
      for (let col = bounds.startCol; col <= bounds.endCol; col++) {
        setCellStyle(row, col, styleProps);
      }
    }
    refresh++;
  }

  // Example function to set some demo styles
  function setDemoStyles() {
    // Header row
    for (let col = 0; col < 5; col++) {
      setCellStyle(0, col, {
        backgroundColor: '#4CAF50',
        color: '#ffffff',
        fontWeight: 'bold',
        textAlign: 'center'
      });
    }
    
    // Alternate row colors
    for (let row = 1; row < 10; row++) {
      const bgColor = row % 2 === 0 ? '#f5f5f5' : '#ffffff';
      for (let col = 0; col < 5; col++) {
        setCellStyle(row, col, {
          backgroundColor: bgColor
        });
      }
    }
    
    // Highlighted cell
    setCellStyle(5, 2, {
      backgroundColor: '#ffeb3b',
      fontWeight: 'bold'
    });
  }

  // resizing 
  let isResizing = $state(false);
  let resizeTarget = null; // {type: 'col'|'row', index: number}
  /** @type Object 
   *  @member ?colIndex
   * */ 
  let columnWidths = {};   // {colIndex: width}
  /** @type Object 
   *  @member ?rowIndex
   * */ 
  let rowHeights = {};     // {rowIndex: height}
  let resizeStartPos = 0;
  let resizeStartSize = 0;
  let cursorStyle = $state('cell');

  function resizeCanvas() {
    // if (canvas && container) {
    if (canvas) {
      // canvas.width = container.clientWidth;
      // canvas.height = container.clientHeight;
      refresh++;
    }
  }

  function getColumnWidth(col) {
    return columnWidths[col] || CELL_WIDTH;
  }

  function getRowHeight(row) {
    return rowHeights[row] || CELL_HEIGHT;
  }

  function getColumnX(col) {
    let x = 0;
    for (let i = 0; i < col; i++) {
      x += getColumnWidth(i);
    }
    return x;
  }

  function getRowY(row) {
    let y = 0;
    for (let i = 0; i < row; i++) {
      y += getRowHeight(i);
    }
    return y;
  }

  function getColumnFromX(x) {
    let currentX = 0;
    let col = 0;
    while (currentX < x) {
      currentX += getColumnWidth(col);
      col++;
    }
    return col - 1;
  }

  function getRowFromY(y) {
    let currentY = 0;
    let row = 0;
    while (currentY < y) {
      currentY += getRowHeight(row);
      row++;
    }
    return row - 1;
  }

  function getResizeHandle(x, y) {

    // Check column resize handles
    if (y < HEADER_HEIGHT && y > 0) {
      let currentX = ROW_HEADER_WIDTH;
      let col = getColumnFromX(scrollX);
      
      while (currentX < canvas.canvas.width) {
        const colX = getColumnX(col) - scrollX + ROW_HEADER_WIDTH;
        const colWidth = getColumnWidth(col);
        const rightEdge = colX + colWidth;
        
        if (Math.abs(x - rightEdge) < RESIZE_HANDLE_SIZE) {
          return { type: 'col', index: col };
        }
        
        currentX = rightEdge;
        col++;
      }
    }
    
    // Check row resize handles
    if (x < ROW_HEADER_WIDTH && x > 0) {
      let currentY = HEADER_HEIGHT;
      let row = getRowFromY(scrollY);
      while (currentY < canvas.canvas.height) {
        const rowY = getRowY(row) - scrollY + HEADER_HEIGHT;
        const rowHeight = getRowHeight(row);
        const bottomEdge = rowY + rowHeight;
        if (Math.abs(y - bottomEdge) < RESIZE_HANDLE_SIZE) {
          return { type: 'row', index: row };
        }
        currentY = bottomEdge;
        row++;
      }
    }

    return null;
  }


  // cell selection
  let selectedCell   = null;
  let selectionStart = $state(null);
  let selectionEnd   = $state(null);
  let isDragging     = false;
  let erasorState    = $state(false)

  /** 
   * @typedef  EditingCell
   * @type     {object}
   * @property {number} row 
   * @property {number} col
   * */

  /** @type {EditingCell} */
  let editingCell = $state(null);

  let editValue = $state('');

  // prevent refetching data by caching values in a set
  let visibleCells = new Set();

  let funcBudgetWidth  = $state(0)
  let funcBlockHeight  = $state(80);

  let funcText  = $state('=(ij -> ji | write<A1>, @rand<100,[10,10]>)')
  let funcStyle = $derived ({
    position: 'fixed',
    bottom: `0px`,
    height: `${funcBlockHeight}px`,
    width:  `${funcBudgetWidth}px`,
    left:   `${DRAWER}px`
  });

  let editorStyle = $derived( editingCell ? {
    position: 'absolute',
    // left:    `${editingCell.col * CELL_WIDTH  - scrollX + ROW_HEADER_WIDTH + DRAWER}px`,
    // top:     `${editingCell.row * CELL_HEIGHT - scrollY + HEADER_HEIGHT}px`,
    left:    `${getColumnX(editingCell.col) - scrollX + ROW_HEADER_WIDTH + DRAWER}px`,
    top:     `${getRowY(editingCell.row) - scrollY + HEADER_HEIGHT}px`,
    // width:   `${CELL_WIDTH}px`,
    // height:  `${CELL_HEIGHT}px`,
    width: `${getColumnWidth(editingCell.col) - 2}px`,
    height: `${getRowHeight(editingCell.row) - 2}px`,
    // width:   `${CELL_WIDTH}px`,
    // height:  `${CELL_HEIGHT}px`,
    border:  '2px solid #1976d2',
    padding: '4px',
    fontSize: `${defaultStyle.fontSize - 2} px`,
    fontFamily: 'sans-serif',
    outline: 'none',
    zIndex: '10'
  } : { display: 'none' });

  function getColumnLabel(col) {
    let label = [];
    let num   = col;
    while (num >= 0) {
      label.push(String.fromCharCode(65 + (num % 26)));
      num = Math.floor(num / 26) - 1;
    }
    return label.join('');
  }

  function erase(rowstart, colstart, rowend, colend) { 
    controller.myLib.griderase(rowstart, colstart, rowend, colend)
  }
  
  function handleWheel(e) {
    e.preventDefault();
    scrollX = Math.max(0, scrollX + e.deltaX);
    scrollY = Math.max(0, scrollY + e.deltaY);
  }

  // Callback that gets called when a cell comes into view
  function onCellVisible(row, col) {
      const key = `${row},${col}`;
      if (!visibleCells.has(key)) {
          //  console.log(`Cell (${row}, ${col}) came into view - fetch data here`);
          //  actual async data fetching
          if (!cellData[key]) {
              cellData[key] = controller.myLib.get(row, col);
          }
          visibleCells.add(key);
      }
  }

  function getCellFromPosition(x, y) {
      if (x < ROW_HEADER_WIDTH || y < HEADER_HEIGHT) return null;

      // const col = Math.floor((x - ROW_HEADER_WIDTH + scrollX) / CELL_WIDTH);
      // const row = Math.floor((y - HEADER_HEIGHT + scrollY) / CELL_HEIGHT);
      const col = getColumnFromX(x - ROW_HEADER_WIDTH + scrollX);
      const row = getRowFromY(y - HEADER_HEIGHT + scrollY);

      return { row, col };
  }

  function getSelectionBounds() {
      if (!selectionStart || !selectionEnd) return null;

      return {
          startRow: Math.min(selectionStart.row, selectionEnd.row),
          endRow:   Math.max(selectionStart.row, selectionEnd.row),
          startCol: Math.min(selectionStart.col, selectionEnd.col),
          endCol:   Math.max(selectionStart.col, selectionEnd.col)
      };
  }


  /** 
   * @param {number} row 
   * @param {number} col */
  function isCellSelected(row, col) {
      const bounds = getSelectionBounds();
      if (!bounds) return false;

      return row >= bounds.startRow && row <= bounds.endRow &&
          col >= bounds.startCol && col <= bounds.endCol;
  }

  // Dragging and selection
  function handleMouseDown(e) {
      // ensure plots don't block
      e.stopPropagation();

      const rect = e.target.getBoundingClientRect();
      const x = e.clientX - rect.left;
      const y = e.clientY - rect.top;

      // Check if clicking on resize handle
      const handle = getResizeHandle(x, y);
      if (handle) {
          isResizing = true;
          resizeTarget = handle;
          if (resizeTarget.index < 0) {
            resizeTarget.index = 0;
          }
          resizeStartPos = handle.type === 'col' ? x : y;
          resizeStartSize = handle.type === 'col' 
              ? getColumnWidth(handle.index) 
              : getRowHeight(handle.index);
          return;
      }

      const cell = getCellFromPosition(x, y);
      if (cell) {
          selectedCell = cell;
          selectionStart = cell;
          selectionEnd = cell;
          isDragging = true;
          editingCell = null;
          refresh++;
      }
  }

  function handleMouseMove(e) {
      // ensure plots don't block

      const rect = e.target.getBoundingClientRect();
      const x = e.clientX - rect.left;
      const y = e.clientY - rect.top;


      // Update cursor based on hover
      const handle = getResizeHandle(x, y);
      if (handle) {
        cursorStyle = handle.type === 'col' ? 'col-resize' : 'row-resize';
      } else {
        cursorStyle = 'cell';
      }

      if (isResizing && resizeTarget) {
          const currentPos = resizeTarget.type === 'col' ? x : y;
          const delta = currentPos - resizeStartPos;
          const newSize = Math.max(20, resizeStartSize + delta);
          if (resizeTarget.type === 'col') {
            columnWidths[resizeTarget.index] = newSize;
          } else {
            rowHeights[resizeTarget.index] = newSize;
          }
          refresh++;
          return;
      }

      if (!isDragging) return;

      const cell = getCellFromPosition(x, y);
      if (cell && (cell.row !== selectionEnd?.row || cell.col !== selectionEnd?.col)) {
          selectionEnd = cell;
      }

      refresh++;
  }

  function handleMouseUp() {
      isDragging = false;
      isResizing = false;
      resizeTarget = null;

      if (selectionStart) {
          const rowstart = selectionStart.row;
          const rowend   = selectionEnd.row;
          const colstart = selectionStart.col;
          const colend   = selectionEnd.col;
          var i,j;

          if (erasorState) {
              // erase cells and force refetch
              erase(rowstart, colstart, rowend, colend);
              if (rowstart <= rowend) {
                  for(i = rowstart; i <= rowend; i++) {
                      if (colstart <= colend) {
                          for(j = colstart; j <= colend; j++) {
                              const key = `${i},${j}`;
                              delete cellData[key];
                              visibleCells.delete(key);
                          }
                      } else {
                          for(var j = colend; j <= colstart; j++) {
                              const key = `${i},${j}`;
                              delete cellData[`${i},${j}`];
                              visibleCells.delete(key);
                          }
                      }
                  }
              } else {
                  for(i = rowend; i <= rowstart; i++) {
                      if (colstart <= colend) {
                          for(j = colstart; j <= colend; j++) {
                              const key = `${i},${j}`;
                              delete cellData[key];
                              visibleCells.delete(key);
                          }
                      } else {
                          for(var j = colend; j <= colstart; j++) {
                              const key = `${i},${j}`;
                              delete cellData[`${i},${j}`];
                              visibleCells.delete(key);
                          }
                      }
                  }
              }

              // toggle and reset styles for selections
              // erasorState = false;
              // selectStyle.borderColor = '#1976d2'; 
              // selectStyle.foregroundColor = '#cce5ff'; 
              // selectStyle.borderWidth = 2;
              refresh++;
          }
      }
  }

  // function handleClick(e) {
  //     if (isDragging) return
  //     const rect = e.target.getBoundingClientRect();
  //     const x = e.clientX - rect.left;
  //     const y = e.clientY - rect.top;

  //     const cell = getCellFromPosition(x, y);
  //     if (cell) {
  //         const cellKey = `${cell.row},${cell.col}`;
  //         selectionStart = cell;
  //         selectionEnd = cell;
  //     }
  //     counter++;
  // }

  /** @param {ClipboardEvent} e */
  function handlePaste(e) {
    // Stop data actually being pasted into div
    e.stopPropagation();
    e.preventDefault();
    const data = e.clipboardData?.getData('Text');
    if (selectedCell != null && selectedCell != undefined) {
        visibleCells.clear();
        cellData = {};
        controller.myLib.paste(selectedCell.row, selectedCell.col, data);
        if (editingCell != null) {
            onCellVisible(selectedCell.row, selectedCell.col);
            const key = `${selectedCell.row},${selectedCell.col}`;
            editValue = cellData[key]
        }
        refresh++;
    } else {
        console.error("missing paste location!")
    }
  }

  function handleDoubleClick(e) {
      const rect = e.target.getBoundingClientRect();
      const x = e.clientX - rect.left;
      const y = e.clientY - rect.top;

      const cell = getCellFromPosition(x, y);
      if (cell) {
          const cellKey = `${cell.row},${cell.col}`;
          editingCell = cell;
          selectionStart = cell;
          selectionEnd = cell;
          // make sure style menu has proper style
          const style = getCellStyle(cell.row, cell.col);
          styleBuffer = { ...style };
          editValue = cellData[cellKey] || '';
          setTimeout(() => editor?.focus(), 0);
      }
      refresh++;
  }

  
  function handleEditorBlur() {
    if (editingCell) {
      const cellKey = `${editingCell.row},${editingCell.col}`;
      const editOut = editValue.trim();
      if (editOut.startsWith('=')) {
        const num = parseFloat(editOut.substring(1));  
        controller.myLib.gridaddnumber(editingCell.row, editingCell.col, num);
      } else {
        const num = parseFloat(editOut.substring(1));  
        if (isNaN(num)) {
            controller.myLib.gridaddstring(editingCell.row, editingCell.col, editOut);
        } else {
            controller.myLib.gridaddnumber(editingCell.row, editingCell.col, num);
        }
      }
      cellData[cellKey] = editValue;
      editingCell = null;
      refresh++;
    }
  }

  /** @param {KeyboardEvent} e  */
  function handleEditorKeyDown(e) {
    if (e.key === 'Enter' && !e.shiftKey) {
      e.preventDefault();
      handleEditorBlur();
    } else if (e.key === 'Escape') {
      editingCell = null;
      editValue = '';
    }
  }

  function getShapeLabels() {
    const bounds = getSelectionBounds();
    if (!bounds) return 'None';
    
    if (bounds.startRow === bounds.endRow && bounds.startCol === bounds.endCol) {
      return `${getColumnLabel(bounds.startCol)}${bounds.startRow + 1}`;
    }
    
    return `${getColumnLabel(bounds.startCol)}${bounds.startRow + 1}..${getColumnLabel(bounds.endCol)}${bounds.endRow + 1}`;
  }

  function getShape() {
    const bounds = getSelectionBounds();
    if (!bounds) return '??';
    
    const rows = bounds.endRow - bounds.startRow + 1;
    const cols = bounds.endCol - bounds.startCol + 1;
    return `${rows}×${cols}`;
  }

  let selectionLabel = $derived(selectionEnd ? getShapeLabels() : '');
  let selectionSize  = $derived(selectionEnd ? getShape() : '');

</script>

<!--<svelte:window />-->
<div class="drawer fixed mt-26 drawer-open" >
    <input id="my-drawer-4" type="checkbox" class="drawer-toggle" />
    <div class="drawer-content" bind:this={plotArea} >

        <div class='h-[90%] w-[100vw]'>

            <!--TODO: wheel and touch events may not work as well on layers - find out why ?? --->
            <input
                bind:this={editor}
                type="text"
                bind:value={editValue}
                onblur    ={handleEditorBlur}
                onkeydown ={handleEditorKeyDown}
                onpaste={handlePaste}
                class="bg-black text-white text-xs "
                style={Object.entries(editorStyle).map(([k, v]) => `${k}: ${v}`).join('; ')}
            />
            
            <div style={`
              position: absolute;
              top: 32px;
              right: 12px;
              background: white;
              padding: 10px;
              border-radius: 4px;
              font-size: 12px;
            `} class="shadow-2xl w-38 ">
            <div class="flex flex-col items-center"> 
                <div class="text-lg">{selectionLabel}</div>
                <div class="text-xs">{selectionSize}</div>
                <div class="flex flex-row px-8 w-full items-center justify-around">
                    <div> 
                        <i class="fa fa-angle-double-left fa-xs text-black text-center" aria-hidden="true"></i>
                        {Math.round(scrollX)}
                    </div>
                    <div> 
                        <i class="fa fa-angle-double-up fa-xs text-black text-center" aria-hidden="true"></i>
                        {Math.round(scrollY)}
                    </div>
                </div>
            </div>
        </div>

            <!--<div bind:this={overlay} class="pointer-events-none h-[90%] w-[100vw]">-->
            <!--</div>-->

            <!--layerEvents-->
            <Canvas
                bind:this={canvas}
                ondblclick ={handleDoubleClick} 
                onmousedown={handleMouseDown} 
                onmouseup  ={handleMouseUp}
                onmousemove={handleMouseMove}
                onwheel={handleWheel} 
                style={`display: block; cursor: ${cursorStyle};`}>
                {#key refresh}
                    <Layer 
                        render={ ({ context, width, height }) => { 
                            // see: https://github.com/sveltejs/svelte/issues/15066
                            // see: https://github.com/sveltejs/svelte/issues/2068
                            funcBudgetWidth  = width;

                            // Clear canvas
                            context.clearRect(0, 0, width, height);

                            // Calculate visible range
                            // const startCol = Math.floor(scrollX / CELL_WIDTH);
                            // const endCol   = Math.ceil((scrollX + width) / CELL_WIDTH);
                            // const startRow = Math.floor(scrollY / CELL_HEIGHT);
                            // const endRow   = Math.ceil((scrollY + height) / CELL_HEIGHT);

                            // TODO: simplify clamping behaviour - logic is spread about
                            let startCol = getColumnFromX(scrollX);
                            if (startCol < 0) { startCol = 0; }

                            let startRow = getRowFromY(scrollY);
                            if (startRow < 0) { startRow = 0; }
                             
                            let endCol = startCol;
                            let colX = getColumnX(startCol);
                            while (colX < scrollX + width) {
                               colX += getColumnWidth(endCol);
                               endCol++;
                            }
                            if (endCol < 0) { endCol = 0; }

                            let endRow = startRow;
                            let rowY = getRowY(startRow);
                            while (rowY < scrollY + height) {
                               rowY += getRowHeight(endRow);
                               endRow++;
                            }
                            if (endRow < 0) { endRow = 0; }

                            const bounds = getSelectionBounds();

                            // Draw cells
                            // context.strokeStyle = '#ddd';
                            // context.fillStyle   = '#000';
                            // context.font        = '12px sans-serif';

                            for (let row = startRow; row <= endRow; row++) {
                                for (let col = startCol; col <= endCol; col++) {

                                    //const x = col * CELL_WIDTH - scrollX + ROW_HEADER_WIDTH;
                                    //const y = row * CELL_HEIGHT - scrollY + HEADER_HEIGHT;

                                    const x = getColumnX(col) - scrollX + ROW_HEADER_WIDTH;
                                    const y = getRowY(row) - scrollY + HEADER_HEIGHT;
                                    const w = getColumnWidth(col);
                                    const h = getRowHeight(row);

                                    // Call visibility callback
                                    onCellVisible(row, col);

                                    // Get cell style
                                    const style = getCellStyle(row, col);

                                    // Draw cell background
                                    context.fillStyle = style.backgroundColor;
                                    context.fillRect(x, y, w, h);

                                    // Highlight selected cells
                                    if (isCellSelected(row, col)) {
                                        // context.fillStyle = '#e3f2fd';
                                        context.fillStyle = selectStyle.foregroundColor;
                                        // context.fillRect(x, y, CELL_WIDTH, CELL_HEIGHT);
                                        context.fillRect(x, y, w, h);
                                        context.fillStyle = '#000';
                                    }

                                    // Draw cell border
                                    // context.strokeRect(x, y, CELL_WIDTH, CELL_HEIGHT);
                                    context.strokeStyle = style.borderColor;
                                    context.lineWidth = style.borderWidth;
                                    context.strokeRect(x, y, w, h);

                                    // Draw cell content
                                    const cellKey = `${row},${col}`;
                                    const cellValue = cellData[cellKey] || '';
                                    if (cellValue) {
                                        context.fillStyle = '#000';
                                        // context.font = 'bold 16px sans-serif';
                                        // context.fillText(cellValue, x + 5, y + 20);
                                        context.save();
                                        context.beginPath();
                                        context.rect(x, y, w, h);
                                        context.clip();

                                         // Apply text styles
                                        context.fillStyle = style.color;
                                        context.font = `${style.fontStyle} ${style.fontWeight} ${style.fontSize}px sans-serif`;
                                        // context.fillStyle = '#000';

                                         // Calculate text position based on alignment
                                        let textX = x + 5;
                                        if (style.textAlign === 'center') {
                                          textX = x + w / 2;
                                          context.textAlign = 'center';
                                        } else if (style.textAlign === 'right') {
                                          textX = x + w - 5;
                                          context.textAlign = 'right';
                                        } else {
                                          context.textAlign = 'left';
                                        }
                                        
                                        let textY = y + h / 2 + style.fontSize / 3;
                                        if (style.verticalAlign === 'top') {
                                          textY = y + style.fontSize + 2;
                                        } else if (style.verticalAlign === 'bottom') {
                                          textY = y + h - 5;
                                        }
 
                                        context.fillText(cellValue, textX, textY);
                                        // context.fillText(cellValue, x + 5, y + (h / 2) + 5);
 
                                        // Draw text decoration
                                        if (style.textDecoration === 'underline') {
                                          const metrics = context.measureText(cellValue);
                                          context.beginPath();
                                          context.moveTo(textX - (style.textAlign === 'center' ? metrics.width / 2 : 0), textY + 2);
                                          context.lineTo(textX + (style.textAlign === 'center' ? metrics.width / 2 : metrics.width), textY + 2);
                                          context.strokeStyle = '#000000';
                                          context.stroke();
                                        }

                                        context.restore();
                                    }
                                }
                            }

                            // Draw selection border
                            if (bounds) {
                                //const x =  bounds.startCol * CELL_WIDTH - scrollX + ROW_HEADER_WIDTH;
                                //const y =  bounds.startRow * CELL_HEIGHT - scrollY + HEADER_HEIGHT;
                                //const w = (bounds.endCol - bounds.startCol + 1) * CELL_WIDTH;
                                //const h = (bounds.endRow - bounds.startRow + 1) * CELL_HEIGHT;

                                const x = getColumnX(bounds.startCol) - scrollX + ROW_HEADER_WIDTH;
                                const y = getRowY(bounds.startRow) - scrollY + HEADER_HEIGHT;
                                let w = 0;
                                for (let col = bounds.startCol; col <= bounds.endCol; col++) {
                                  w += getColumnWidth(col);
                                }
                                let h = 0;
                                for (let row = bounds.startRow; row <= bounds.endRow; row++) {
                                  h += getRowHeight(row);
                                }

                                context.strokeStyle = selectStyle.borderColor;
                                context.lineWidth   = selectStyle.borderWidth;
                                context.strokeRect(x, y, w, h);
                            }

                            // Draw column headers
                            context.fillStyle = '#f5f5f5';
                            context.fillRect(ROW_HEADER_WIDTH, 0, width - ROW_HEADER_WIDTH, HEADER_HEIGHT);
                            context.fillStyle = '#000';
                            context.font = 'bold 12px sans-serif';
                            context.strokeStyle = '#ddd';

                            for (let col = startCol; col <= endCol; col++) {
                                //const x = col * CELL_WIDTH - scrollX + ROW_HEADER_WIDTH;
                                const x = getColumnX(col) - scrollX + ROW_HEADER_WIDTH;
                                const w = getColumnWidth(col);


                                // Highlight selected column headers
                                const bounds = getSelectionBounds();
                                if (bounds && col >= bounds.startCol && col <= bounds.endCol) {
                                    // context.fillStyle = '#cce5ff';
                                    context.fillStyle = selectStyle.foregroundColor;
                                    //context.fillRect(x, 0, CELL_WIDTH, HEADER_HEIGHT);
                                    context.fillRect(x, 0, w, HEADER_HEIGHT);
                                    context.fillStyle = '#000';
                                }

                                // context.strokeRect(x, 0, CELL_WIDTH, HEADER_HEIGHT);
                                context.strokeRect(x, 0, w, HEADER_HEIGHT);
                                context.fillText(getColumnLabel(col), x + 5, 20);
                            }

                            // Draw row headers
                            context.fillStyle = '#f5f5f5';
                            context.fillRect(0, HEADER_HEIGHT, ROW_HEADER_WIDTH, height - HEADER_HEIGHT);
                            context.fillStyle = '#000';

                            for (let row = startRow; row <= endRow; row++) {
                                // const y = row * CELL_HEIGHT - scrollY + HEADER_HEIGHT;
                                const y = getRowY(row) - scrollY + HEADER_HEIGHT;
                                const h = getRowHeight(row);

                                // Highlight selected row headers
                                //const bounds = getSelectionBounds();
                                if (bounds && row >= bounds.startRow && row <= bounds.endRow) {
                                    // context.fillStyle = '#cce5ff';
                                    context.fillStyle = selectStyle.foregroundColor;
                                    // context.fillRect(0, y, ROW_HEADER_WIDTH, CELL_HEIGHT);
                                    context.fillRect(0, y, ROW_HEADER_WIDTH, h);
                                    context.fillStyle = '#000';
                                }

                                // context.strokeRect(0, y, ROW_HEADER_WIDTH, CELL_HEIGHT);
                                context.strokeRect(0, y, ROW_HEADER_WIDTH, h);
                                context.fillText(`${(row + 1)}`, 5, y + (h/2) +5);
                            }

                            // Draw corner
                            context.fillStyle = '#f5f5f5';
                            context.fillRect(0, 0, ROW_HEADER_WIDTH, HEADER_HEIGHT);
                            context.strokeRect(0, 0, ROW_HEADER_WIDTH, HEADER_HEIGHT);
                        }} />
                {/key}
            </Canvas>

            <div class="flex flex-row bg-transparent backdrop-blur-sm border-t
                border-t-black resize-y" 
                style={Object.entries(funcStyle).map(([k, v]) => `${k}: ${v}`).join('; ')}>
                <div class="flex bg-black items-center basis-4">
                    <span class="p-4 text-center text-white">ƒ𝑥</span>
                </div>
                <textarea  
                    class="font-light basis-8 px-4 py-2 text-[14px] resize-none 
                    min-w-full 
                    text-black italic text-area textarea-neutral rounded-none"
                    bind:value={funcText}
                    onkeydown={(e) => {
                        if (e.key === 'Enter' && !e.shiftKey) {
                            if (funcText.startsWith('=')) {
                                controller.myLib.executecode(funcText.substring(1));
                            } else {
                                controller.myLib.executecode(funcText);
                            }
                            // NB: this forces a refetch of data from the grid model
                            visibleCells.clear();
                            cellData = {};
                        }
                    }}
                ></textarea>
            </div>
        </div>

    </div>

    <div class="drawer-side is-drawer-close:overflow-visible  border-r border-r-black">
        <label for="my-drawer-4" aria-label="close sidebar" class="drawer-overlay"></label>
        <div class="is-drawer-close:w-14 is-drawer-open:w-64 bg-base-200 
            flex flex-col items-start min-h-full">
            <!-- Sidebar content here -->
            <ul class="menu w-full grow gap-2">

                <!-- list item -->
                <li class="">
                    <button class="text-md is-drawer-close:tooltip is-drawer-close:tooltip-right" data-tip="Homepage">
                        <i class="fa fa-th  text-gray-300" aria-hidden="true"></i>
                        <span class="is-drawer-close:hidden">Help</span>
                    </button>
                </li>

                <!-- list item -->
                <li >
                    <button class="text-md is-drawer-close:tooltip
                        is-drawer-close:tooltip-right" data-tip="Help">
                        <i class="fa fa-question-circle text-gray-300" aria-hidden="true"></i>
                        <span class="is-drawer-close:hidden">Help</span>
                    </button>
                </li>

                <li>
                    <button onclick={() => { 
                                erasorState = !erasorState;
                                cursorStyle = "crosshair";
                                if (erasorState) {
                                    selectStyle.borderColor = 'red'; 
                                    selectStyle.borderWidth = 1;
                                    selectStyle.foregroundColor = '#fcc7ce'; 
                                    refresh++;
                                } else {
                                    selectStyle.borderColor = '#1976d2'; 
                                    selectStyle.borderWidth = 1;
                                    selectStyle.foregroundColor = '#cce5ff'; 
                                    refresh++;
                                }
                            }} 
                        class={`text-md is-drawer-close:tooltip
                        is-drawer-close:tooltip-right 
                        ${erasorState ? "border border-red-400" : "" }`} 
                        data-tip="Eraser mode">
                        <i class={`fa fa-eraser ${erasorState ? "text-red-400" : "" }`} 
                            aria-hidden="true"></i>
                        <span class="is-drawer-close:hidden">Erase cells</span>
                    </button>
                </li>

                <li class="dropdown dropdown-right dropdown-center">
                    <button tabindex="0" class="text-md is-drawer-close:tooltip
                        is-drawer-close:tooltip-right" data-tip="Text style">
                        <i class="fa fa-font" aria-hidden="true"></i>
                        <span class="is-drawer-close:hidden">Text Style</span>
                    </button>
                    <div tabindex="-1" class="dropdown-content menu bg-base-100
                        shadow-xl transition-shadow duration-150
                        hover:shadow-primary-blue/50 rounded-box z-1 w-56 p-2
                        items-center justify-center mx-4">
                        <div class="menu flex flex-col items-center"> 
                            <h3 class="menu-title text-black text-lg">Text Style</h3>
                            <div class="divider py-0 my-0"></div>
                            <div class="flex flex-row py-4 px-3 items-center justify-between w-full">
                                <input id="fontsize" 
                                    class="basis-2/3 range range-xs range-neutral" type="range" min="10" max="20" step="1" 
                                    bind:value={styleBuffer.fontSize}
                                    onchange={() => {
                                        applyStyleToSelection({ fontSize: styleBuffer.fontSize })
                                    }}  />
                                <label for="fontsize" class="basis-1/3 w-full items-center text-center px-2.5 text-md"> 
                                    <span>{styleBuffer.fontSize} px</span>
                                </label>
                            </div> 
                            <div class="flex flex-row py-2 px-6 justify-between w-full">
                                <button aria-label="none" class="px-3 border border-black rounded  text-md tooltip tooltip-bottom" 
                                    data-tip="Bold" onclick={()=> {
                                        if(styleBuffer.fontWeight == 'bold'){
                                            styleBuffer.fontWeight = 'normal';
                                        } else {
                                            styleBuffer.fontWeight = 'bold';
                                        }
                                        applyStyleToSelection({ fontWeight: styleBuffer.fontWeight });
                                    }}>
                                  <i class="fa fa-bold fa-xs" aria-hidden="true"></i>
                                </button>
                                <button aria-label="none" class="px-3 border border-black rounded text-md tooltip
                                    tooltip-bottom" data-tip="Italic" onclick={()=> {
                                        if(styleBuffer.fontStyle == 'italic'){
                                            styleBuffer.fontStyle = 'normal';
                                        } else {
                                            styleBuffer.fontStyle = 'italic';
                                        }
                                        applyStyleToSelection({ fontStyle: styleBuffer.fontStyle });
                                    }}>
                                  <i class="fa fa-italic fa-xs" aria-hidden="true"></i>
                                </button>
                                <button aria-label="none" class="px-3 border border-black rounded  text-md tooltip
                                    tooltip-bottom" data-tip="Underline" onclick={()=> {
                                        if(styleBuffer.textDecoration == 'none'){
                                            styleBuffer.textDecoration = 'underline';
                                        } else {
                                            styleBuffer.textDecoration = 'none';
                                        }
                                        applyStyleToSelection({ textDecoration: styleBuffer.textDecoration });
                                    }}>
                                  <i class="fa fa-underline fa-xs" aria-hidden="true"></i>
                                </button>
                            </div>
                            <div class="flex flex-row py-2 px-6 justify-between w-full">
                                <button aria-label="none" class="px-2 border border-black rounded  text-md tooltip tooltip-bottom" 
                                    data-tip="Align Left" onclick={()=> {
                                        if(styleBuffer.textAlign != 'left'){
                                            styleBuffer.textAlign = 'left';
                                        } else {
                                            styleBuffer.textAlign = 'center';
                                        }
                                        applyStyleToSelection({ textAlign: styleBuffer.textAlign });
                                    }}>
                                  <i class="fa fa-align-left fa-xs" aria-hidden="true"></i>
                                </button>
                                <button aria-label="none" class="px-2 border border-black rounded text-md tooltip
                                    tooltip-bottom" data-tip="Align Center" onclick={()=> {
                                        styleBuffer.textAlign = 'center';
                                        applyStyleToSelection({ textAlign: styleBuffer.textAlign });
                                    }}>
                                  <i class="fa fa-align-center fa-xs" aria-hidden="true"></i>
                                </button>
                                <button aria-label="none" class="px-2 border border-black rounded  text-md tooltip
                                    tooltip-bottom" data-tip="Align Right" onclick={()=> {
                                        if (styleBuffer.textAlign != 'right') {
                                            styleBuffer.textAlign = 'right';
                                        } else {
                                            styleBuffer.textAlign = 'center';
                                        }
                                        applyStyleToSelection({ textAlign: styleBuffer.textAlign });
                                    }}>
                                  <i class="fa fa-align-right fa-xs" aria-hidden="true"></i>
                                </button>
                            </div>
                            <div class="flex flex-row py-2 px-6 justify-between w-full">
                                <input id="fontcolor" 
                                    class="basis-2/3 range range-xs range-neutral" 
                                    type="color"
                                    bind:value={styleBuffer.color}
                                    onchange={() => {
                                        applyStyleToSelection({ color: styleBuffer.color })
                                    }}  />
                                <label for="fontcolor" class="basis-1/3 w-full items-center text-center px-2.5 text-md"> 
                                    <span>{styleBuffer.color}</span>
                                </label>
                            </div>
                        </div>
                    </div>
                </li>

                <li class="dropdown dropdown-right dropdown-center">
                    <button tabindex="0" class="text-md is-drawer-close:tooltip
                        is-drawer-close:tooltip-right" data-tip="Table Style">
                        <i class="fa fa-table" aria-hidden="true"></i>
                        <span class="is-drawer-close:hidden">Table Style</span>
                    </button>
                    <div tabindex="-1" class="dropdown-content menu bg-base-100
                        shadow-xl transition-shadow duration-150
                        hover:shadow-primary-blue/50 rounded-box z-1 w-56 p-2
                        items-center justify-center mx-4">
                        <div class="menu flex flex-col items-center"> 
                            <h3 class="menu-title text-black text-lg">Cell Style</h3>
                            <div class="divider py-0 my-0"></div>
                            <h3 class="text-black text-sm">Border Width</h3>
                            <div class="flex flex-row py-4 px-3 items-center justify-between w-full">
                                <input id="bordersize" 
                                    class="basis-2/3 range range-xs range-neutral" type="range" min="1" max="4" step="1" 
                                    bind:value={styleBuffer.borderWidth}
                                    onchange={() => {
                                        applyStyleToSelection({ borderWidth: styleBuffer.borderWidth })
                                    }} />
                                <label for="bordersize" class="basis-1/3 w-full items-center text-center px-2.5 text-md"> 
                                    <span>{styleBuffer.borderWidth} em </span>
                                </label>
                            </div>
                            <h3 class="text-black text-xs">Background Color</h3>
                            <div class="flex flex-row py-2 px-6 justify-between w-full">
                                <input id="backgroundcolor" 
                                    class="basis-2/3 range range-xs range-neutral" 
                                    type="color"
                                    bind:value={styleBuffer.backgroundColor}
                                    onchange={() => {
                                        applyStyleToSelection({ backgroundColor: styleBuffer.backgroundColor })
                                    }}  />
                                <label for="backgroundcolor" class="basis-1/3 w-full items-center text-center px-2.5 text-md"> 
                                    <span>{styleBuffer.backgroundColor }</span>
                                </label>
                            </div>
                            <h3 class="text-black text-sm">Border Color</h3>
                            <div class="flex flex-row py-2 px-6 justify-between w-full">
                                <input id="bordercolor" 
                                    class="basis-2/3 range range-xs range-neutral" 
                                    type="color"
                                    bind:value={styleBuffer.borderColor}
                                    onchange={() => {
                                        applyStyleToSelection({ borderColor: styleBuffer.borderColor })
                                    }}  />
                                <label for="bordercolor" class="basis-1/3 w-full items-center text-center px-2.5 text-md"> 
                                    <span>{styleBuffer.borderColor}</span>
                                </label>
                            </div>
                        </div>
                    </div>
                </li>

                <li>
                    <button class="text-md is-drawer-close:tooltip
                        is-drawer-close:tooltip-right" data-tip="Import">
                        <i class="fa fa-upload text-gray-300" aria-hidden="true"></i>
                        <span class="is-drawer-close:hidden">Import</span>
                    </button>
                </li>

            </ul>
        </div>
    </div>
</div>


