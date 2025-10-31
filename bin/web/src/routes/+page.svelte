<script>
  import { Canvas, Layer } from 'svelte-canvas';
  // TODO: standardize controller so that we can type it
  import { default as controller } from '$lib/index';
  import { onMount } from 'svelte';

  // editing support via floating input 
  let editor
  let plotArea 

  onMount(() => {
    controller.myLib.renderarea(plotArea);
  })

  let refresh = $state(0);

  const CELL_WIDTH       = 120;
  const CELL_HEIGHT      = 30;
  const HEADER_HEIGHT    = 30;
  const ROW_HEADER_WIDTH = 60;

  // needed to render the editing cell absolutelt
  const DRAWER           = 57; 

  // Scroll computation
  let scrollX =  $state(0);
  let scrollY =  $state(0);
  let cellData = $state({})

  // cell selection
  let selectedCell   = null;
  let selectionStart = null;
  let selectionEnd   = null;
  let isDragging     = false;

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

  let funcText  = $state('=(ij -> ji | zscore | write<A11>, @A1..J10)')
  let funcStyle = $derived ({
    position: 'fixed',
    bottom: `0px`,
    height: `${funcBlockHeight}px`,
    width:  `${funcBudgetWidth}px`,
    left:   `${DRAWER}px`
  });

  let editorStyle = $derived( editingCell ? {
    position: 'absolute',
    left:    `${editingCell.col * CELL_WIDTH  - scrollX + ROW_HEADER_WIDTH + DRAWER}px`,
    top:     `${editingCell.row * CELL_HEIGHT - scrollY + HEADER_HEIGHT}px`,
    width:   `${CELL_WIDTH}px`,
    height:  `${CELL_HEIGHT}px`,
    border:  '2px solid #1976d2',
    padding: '4px',
    fontSize: '10px',
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

      const col = Math.floor((x - ROW_HEADER_WIDTH + scrollX) / CELL_WIDTH);
      const row = Math.floor((y - HEADER_HEIGHT + scrollY) / CELL_HEIGHT);

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
      controller.myLib.cleardrag(e);
      e.stopPropagation();
      const rect = e.target.getBoundingClientRect();
      const x = e.clientX - rect.left;
      const y = e.clientY - rect.top;

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
      controller.myLib.cleardrag(e);
      if (!isDragging) return;

      const rect = e.target.getBoundingClientRect();
      const x = e.clientX - rect.left;
      const y = e.clientY - rect.top;

      const cell = getCellFromPosition(x, y);
      if (cell && (cell.row !== selectionEnd?.row || cell.col !== selectionEnd?.col)) {
          selectionEnd = cell;
      }
      refresh++;
  }

  function handleMouseUp() {
      isDragging = false;
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
      controller.myLib.cleardrag(e);
      const rect = e.target.getBoundingClientRect();
      const x = e.clientX - rect.left;
      const y = e.clientY - rect.top;

      const cell = getCellFromPosition(x, y);
      if (cell) {
          const cellKey = `${cell.row},${cell.col}`;
          editingCell = cell;
          selectionStart = cell;
          selectionEnd = cell;
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

            <!--<div bind:this={overlay} class="pointer-events-none h-[90%] w-[100vw]">-->
            <!--</div>-->

            <!--layerEvents-->
            <Canvas
                ondblclick ={handleDoubleClick} 
                onmousedown={handleMouseDown} 
                onmouseup  ={handleMouseUp}
                onmousemove={handleMouseMove}
                onwheel={handleWheel} 
                style="display: block; cursor: cell;">
                {#key refresh}
                    <Layer 
                        render={ ({ context, width, height }) => { 
                            // see: https://github.com/sveltejs/svelte/issues/15066
                            // see: https://github.com/sveltejs/svelte/issues/2068
                            funcBudgetWidth  = width;

                            // Calculate visible range
                            const startCol = Math.floor(scrollX / CELL_WIDTH);
                            const endCol   = Math.ceil((scrollX + width) / CELL_WIDTH);
                            const startRow = Math.floor(scrollY / CELL_HEIGHT);
                            const endRow   = Math.ceil((scrollY + height) / CELL_HEIGHT);

                            const bounds = getSelectionBounds();

                            // Draw cells
                            context.strokeStyle = '#ddd';
                            context.fillStyle   = '#000';
                            context.font        = '12px sans-serif';

                            for (let row = startRow; row <= endRow; row++) {
                                for (let col = startCol; col <= endCol; col++) {
                                    const x = col * CELL_WIDTH - scrollX + ROW_HEADER_WIDTH;
                                    const y = row * CELL_HEIGHT - scrollY + HEADER_HEIGHT;

                                    // Call visibility callback
                                    onCellVisible(row, col);

                                    // Highlight selected cells
                                    if (isCellSelected(row, col)) {
                                        context.fillStyle = '#e3f2fd';
                                        context.fillRect(x, y, CELL_WIDTH, CELL_HEIGHT);
                                        context.fillStyle = '#000';
                                    }

                                    // Draw cell border
                                    context.strokeRect(x, y, CELL_WIDTH, CELL_HEIGHT);

                                    // Draw cell content
                                    const cellKey = `${row},${col}`;
                                    const cellValue = cellData[cellKey] || '';
                                    if (cellValue) {
                                        context.fillStyle = '#000';
                                        // context.font = 'bold 16px sans-serif';
                                        context.fillText(cellValue, x + 5, y + 20);
                                    }
                                }
                            }

                            // Draw selection border
                            if (bounds) {
                                const x =  bounds.startCol * CELL_WIDTH - scrollX + ROW_HEADER_WIDTH;
                                const y =  bounds.startRow * CELL_HEIGHT - scrollY + HEADER_HEIGHT;
                                const w = (bounds.endCol - bounds.startCol + 1) * CELL_WIDTH;
                                const h = (bounds.endRow - bounds.startRow + 1) * CELL_HEIGHT;

                                context.strokeStyle = '#1976d2';
                                context.lineWidth = 2;
                                context.strokeRect(x, y, w, h);
                                context.lineWidth = 1;
                            }

                            // Draw column headers
                            context.fillStyle = '#f5f5f5';
                            context.fillRect(ROW_HEADER_WIDTH, 0, width - ROW_HEADER_WIDTH, HEADER_HEIGHT);
                            context.fillStyle = '#000';
                            context.font = 'bold 12px sans-serif';
                            context.strokeStyle = '#ddd';

                            for (let col = startCol; col <= endCol; col++) {
                                const x = col * CELL_WIDTH - scrollX + ROW_HEADER_WIDTH;

                                // Highlight selected column headers
                                const bounds = getSelectionBounds();
                                if (bounds && col >= bounds.startCol && col <= bounds.endCol) {
                                    context.fillStyle = '#cce5ff';
                                    context.fillRect(x, 0, CELL_WIDTH, HEADER_HEIGHT);
                                    context.fillStyle = '#000';
                                }

                                context.strokeRect(x, 0, CELL_WIDTH, HEADER_HEIGHT);
                                context.fillText(getColumnLabel(col), x + 5, 20);
                            }

                            // Draw row headers
                            context.fillStyle = '#f5f5f5';
                            context.fillRect(0, HEADER_HEIGHT, ROW_HEADER_WIDTH, height - HEADER_HEIGHT);
                            context.fillStyle = '#000';

                            for (let row = startRow; row <= endRow; row++) {
                                const y = row * CELL_HEIGHT - scrollY + HEADER_HEIGHT;

                                // Highlight selected row headers
                                //const bounds = getSelectionBounds();
                                if (bounds && row >= bounds.startRow && row <= bounds.endRow) {
                                    context.fillStyle = '#cce5ff';
                                    context.fillRect(0, y, ROW_HEADER_WIDTH, CELL_HEIGHT);
                                    context.fillStyle = '#000';
                                }

                                context.strokeRect(0, y, ROW_HEADER_WIDTH, CELL_HEIGHT);
                                context.fillText(`${(row + 1)}`, 5, y + 20);
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
                        <i class="fa fa-th" aria-hidden="true"></i>
                        <span class="is-drawer-close:hidden">Help</span>
                    </button>
                </li>

                <!-- list item -->
                <li >
                    <button class="text-md is-drawer-close:tooltip
                        is-drawer-close:tooltip-right" data-tip="Info">
                        <i class="fa fa-question-circle" aria-hidden="true"></i>
                        <span class="is-drawer-close:hidden">Help</span>
                    </button>
                </li>

                <li>
                    <button class="text-md is-drawer-close:tooltip
                        is-drawer-close:tooltip-right" data-tip="Clear">
                        <i class="fa fa-eraser fa" aria-hidden="true"></i>
                        <span class="is-drawer-close:hidden">Clear</span>
                    </button>
                </li>


                <li>
                    <button class="text-md is-drawer-close:tooltip
                        is-drawer-close:tooltip-right" data-tip="Font">
                        <i class="fa fa-font" aria-hidden="true"></i>
                        <span class="is-drawer-close:hidden">Font</span>
                    </button>
                </li>


                <li>
                    <button class="text-md is-drawer-close:tooltip
                        is-drawer-close:tooltip-right" data-tip="Import">
                        <i class="fa fa-upload" aria-hidden="true"></i>
                        <span class="is-drawer-close:hidden">Import</span>
                    </button>
                </li>


                <li>
                    <button class="text-md is-drawer-close:tooltip
                        is-drawer-close:tooltip-right" data-tip="Style">
                        <i class="fa fa-paint-brush " aria-hidden="true"></i>
                        <span class="is-drawer-close:hidden">Style</span>
                    </button>
                </li>

            </ul>
        </div>
    </div>
</div>


